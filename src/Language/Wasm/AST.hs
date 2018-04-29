{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Language.Wasm.AST (

) where

import GHC.TypeLits
import Data.Proxy
import Data.Promotion.Prelude.List

import Language.Wasm.Structure (
        ValueType(..),
        GlobalType(..),
        IUnOp(..),
        IBinOp(..),
        IRelOp(..)
    )

data VType = Val ValueType | Var | Any

type family MatchStack (args :: [VType]) (stack :: [VType]) :: Bool where
    MatchStack (Val I32 : args) (Val I32 : stack) = MatchStack args stack
    MatchStack (Val v : args) (Var : stack) = MatchStack args stack
    MatchStack (Var : args) (val : stack) = MatchStack (ReplaceVar args val) (ReplaceVar stack val)
    MatchStack (val : args) (Var : stack) = MatchStack (ReplaceVar args val) (ReplaceVar stack val)
    MatchStack '[] stack = True
    MatchStack args (Any : stack) = True
    MatchStack args stack = TypeError (
            Text "Cannot match stack with instruction arguments." :$$:
            Text "Expected arguments: " :<>: ShowType args :$$:
            Text "Actual stack: " :<>: ShowType stack
        )

type family Consume (args :: [VType]) (stack :: [VType]) (result :: [VType]) :: [VType] where
    Consume (Val I32 : args) (Val I32 : stack) result = Consume args stack result
    Consume (Var : args) (val : stack) result = Consume (ReplaceVar args val) (ReplaceVar stack val) (ReplaceVar result val)
    Consume (val : args) (Var : stack) result = Consume (ReplaceVar args val) (ReplaceVar stack val) (ReplaceVar result val)
    Consume '[] stack result = result :++ stack
    Consume args (Any : stack) result = result :++ (Any : stack)
    Consume args stack result = TypeError (
            Text "Cannot consume stack." :$$:
            Text "Expected arguments: " :<>: ShowType args :$$:
            Text "Actual stack: " :<>: ShowType stack
        )

type family ReplaceVar (types :: [VType]) (val :: VType) :: [VType] where
    ReplaceVar '[] val = '[]
    ReplaceVar (Var : rest) val = val : ReplaceVar rest val
    ReplaceVar (t : rest) val = t : ReplaceVar rest val

type family GetGlobalType (globalType :: GlobalType) :: VType where
    GetGlobalType (Const vt) = Val vt
    GetGlobalType (Mut vt) = Val vt

type family IsMutable (globalType :: GlobalType) :: Bool where
    IsMutable (Const a) = False
    IsMutable (Mut a) = True

type family IsLabelMatch (label :: Maybe ValueType) (stack :: [VType]) :: Bool where
    IsLabelMatch (Just val) '[Val val] = True
    IsLabelMatch (Just val) '[Any] = True
    IsLabelMatch (Just val) '[Var] = True
    IsLabelMatch Nothing '[] = True
    IsLabelMatch label stack = False

type family LabelAsArgs (label :: Maybe ValueType) :: [VType] where
    LabelAsArgs (Just val) = '[Val val]
    LabelAsArgs Nothing = '[]

class KnownNats ns where
    natVals :: Proxy ns -> [Integer]

instance KnownNats ('[] :: [Nat]) where
    natVals _ = []

instance (KnownNat n, KnownNats ns) => KnownNats (n : ns) where
    natVals p = let (n, ns) = dup p in natVal n : natVals ns
        where
            dup :: Proxy (n : ns) -> (Proxy n, Proxy ns)
            dup _ = (Proxy, Proxy)

data InstrSeq (stack :: [VType]) (locals :: [VType]) (globals :: [GlobalType]) (labels :: [Maybe ValueType]) where
    Empty :: InstrSeq '[] locals globals labels
    Unreachable :: InstrSeq stack locals globals labels -> InstrSeq '[Any] locals globals labels
    Nop :: InstrSeq stack locals globals labels -> InstrSeq stack locals globals labels
    Block :: (IsLabelMatch label result ~ True) =>
        InstrSeq result locals globals (label : labels) ->
        InstrSeq stack locals globals labels ->
        InstrSeq (result :++ stack) locals globals labels
    Loop :: (IsLabelMatch label result ~ True) =>
        InstrSeq result locals globals (label : labels) ->
        InstrSeq stack locals globals labels ->
        InstrSeq (result :++ stack) locals globals labels
    If :: (IsLabelMatch label result ~ True, MatchStack '[Val I32] stack ~ True) =>
        InstrSeq result locals globals (label : labels) ->
        InstrSeq result locals globals (label : labels) ->
        InstrSeq stack locals globals labels ->
        InstrSeq (Consume '[Val I32] stack result) locals globals labels
    Br :: (KnownNat label, MatchStack (LabelAsArgs (labels :!! label)) stack ~ True) =>
        Proxy label ->
        InstrSeq stack locals globals lables ->
        InstrSeq '[Any] locals globals labels
    BrIf :: (KnownNat label, MatchStack ((LabelAsArgs (labels :!! label)) :++ '[Val I32]) stack ~ True) =>
        Proxy label ->
        InstrSeq stack locals globals lables ->
        InstrSeq (Consume ((LabelAsArgs (labels :!! label)) :++ '[Val I32]) stack (LabelAsArgs (labels :!! label))) locals globals labels
    BrTable :: (KnownNat defaultLabel, KnownNats localLabels, MatchStack ((LabelAsArgs (labels :!! defaultLabel)) :++ '[Val I32]) stack ~ True) =>
        Proxy (localLabels :: [Nat]) ->
        Proxy defaultLabel ->
        InstrSeq stack locals globals lables ->
        InstrSeq (Consume ((LabelAsArgs (labels :!! defaultLabel)) :++ '[Val I32]) stack '[Any]) locals globals labels
    Drop :: InstrSeq (any : stack) locals globals labels -> InstrSeq stack locals globals labels
    Select :: (MatchStack '[Var, Var, Val I32] stack ~ True) =>
        InstrSeq stack locals globals labels ->
        InstrSeq (Consume '[Var, Var, Val I32] stack '[Var]) locals globals labels
    GetLocal :: (KnownNat local) =>
        Proxy local ->
        InstrSeq stack locals globals labels ->
        InstrSeq ((locals :!! local) : stack) locals globals labels
    SetLocal :: (KnownNat local, MatchStack '[locals :!! local] stack ~ True) =>
        Proxy local ->
        InstrSeq stack locals globals labels ->
        InstrSeq (Consume '[locals :!! local] stack '[]) locals globals labels
    TeeLocal :: (KnownNat local, MatchStack '[locals :!! local] stack ~ True) =>
        Proxy local ->
        InstrSeq stack locals globals labels ->
        InstrSeq (Consume '[locals :!! local] stack '[locals :!! local]) locals globals labels
    GetGlobal :: (KnownNat global) =>
        Proxy global ->
        InstrSeq stack locals globals labels ->
        InstrSeq ((GetGlobalType (globals :!! global)) : stack) locals globals labels
    SetGlobal :: (KnownNat global, MatchStack '[GetGlobalType (globals :!! global)] stack ~ True, IsMutable (globals :!! global) ~ True) =>
        Proxy global ->
        InstrSeq stack locals globals labels ->
        InstrSeq (Consume '[GetGlobalType (globals :!! global)] stack '[]) locals globals labels
    I32Const :: InstrSeq stack locals globals labels -> InstrSeq (Val I32 : stack) locals globals labels
    I32UnOp :: (MatchStack '[Val I32] stack ~ True) =>
        IUnOp ->
        InstrSeq stack locals globals labels ->
        InstrSeq (Consume '[Val I32] stack '[Val I32]) locals globals labels
    I32BinOp :: (MatchStack '[Val I32, Val I32] stack ~ True) =>
        IBinOp ->
        InstrSeq stack locals globals labels ->
        InstrSeq (Consume '[Val I32, Val I32] stack '[Val I32]) locals globals labels
    I32RelOp :: (MatchStack '[Val I32, Val I32] stack ~ True) =>
        IRelOp ->
        InstrSeq stack locals globals labels ->
        InstrSeq (Consume '[Val I32, Val I32] stack '[Val I32]) locals globals labels