{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

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

type family AsVType (values :: [ValueType]) :: [VType] where
    AsVType (v : vs) = Val v : AsVType vs
    AsVType '[] = '[]

class KnownNats ns where
    natVals :: Proxy ns -> [Integer]

instance KnownNats ('[] :: [Nat]) where
    natVals _ = []

instance (KnownNat n, KnownNats ns) => KnownNats (n : ns) where
    natVals p = let (n, ns) = dup p in natVal n : natVals ns
        where
            dup :: Proxy (n : ns) -> (Proxy n, Proxy ns)
            dup _ = (Proxy, Proxy)

data Ctx (locals :: [VType]) (globals :: [GlobalType]) (labels :: [Maybe ValueType]) (returns :: [ValueType])

type family GetLocals ctx :: [VType] where
    GetLocals (Ctx locals globals labels returns) = locals

type family GetGlobals ctx :: [GlobalType] where
    GetGlobals (Ctx locals globals labels returns) = globals

type family GetLabels ctx :: [Maybe ValueType] where
    GetLabels (Ctx locals globals labels returns) = labels

type family WithLabel ctx (label :: Maybe ValueType) where
    WithLabel (Ctx locals globals labels returns) label = Ctx locals globals (label : labels) returns

type family GetReturns ctx :: [ValueType] where
    GetReturns (Ctx locals globals labels returns) = returns

data InstrSeq (stack :: [VType]) ctx where
    Empty :: InstrSeq '[] ctx
    Unreachable :: InstrSeq stack ctx -> InstrSeq '[Any] ctx
    Nop :: InstrSeq stack ctx -> InstrSeq stack ctx
    Block :: (IsLabelMatch label result ~ True) =>
        Proxy (label :: Maybe ValueType) ->
        InstrSeq result (WithLabel ctx label) ->
        InstrSeq stack ctx ->
        InstrSeq (result :++ stack) ctx
    Loop :: (IsLabelMatch label result ~ True) =>
        Proxy (label :: Maybe ValueType) ->
        InstrSeq result (WithLabel ctx label) ->
        InstrSeq stack ctx ->
        InstrSeq (result :++ stack) ctx
    If :: (IsLabelMatch label result ~ True, MatchStack '[Val I32] stack ~ True) =>
        Proxy (label :: Maybe ValueType) ->
        InstrSeq result (WithLabel ctx label) ->
        InstrSeq result (WithLabel ctx label) ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack result) ctx
    Br :: (KnownNat label, MatchStack (LabelAsArgs ((GetLabels ctx) :!! label)) stack ~ True) =>
        Proxy label ->
        InstrSeq stack ctx ->
        InstrSeq '[Any] ctx
    BrIf :: (KnownNat label, MatchStack ((LabelAsArgs ((GetLabels ctx) :!! label)) :++ '[Val I32]) stack ~ True) =>
        Proxy label ->
        InstrSeq stack ctx ->
        InstrSeq (Consume ((LabelAsArgs ((GetLabels ctx) :!! label)) :++ '[Val I32]) stack (LabelAsArgs ((GetLabels ctx) :!! label))) ctx
    BrTable :: (
            KnownNat defaultLabel,
            KnownNats localLabels,
            MatchStack ((LabelAsArgs ((GetLabels ctx) :!! defaultLabel)) :++ '[Val I32]) stack ~ True
        ) =>
        Proxy (localLabels :: [Nat]) ->
        Proxy defaultLabel ->
        InstrSeq stack ctx ->
        InstrSeq (Consume ((LabelAsArgs ((GetLabels ctx) :!! defaultLabel)) :++ '[Val I32]) stack '[Any]) ctx
    Return :: (MatchStack (AsVType (GetReturns ctx)) stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume (AsVType (GetReturns ctx)) stack '[Any]) ctx
    -- Call :: (MatchStack (AsVType returns) stack ~ True) =>
    --     Proxy function ->
    --     InstrSeq stack locals globals lables returns ->
    Drop :: InstrSeq (any : stack) ctx -> InstrSeq stack ctx
    Select :: (MatchStack '[Var, Var, Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Var, Var, Val I32] stack '[Var]) ctx
    GetLocal :: (KnownNat local) =>
        Proxy local ->
        InstrSeq stack ctx ->
        InstrSeq (((GetLocals ctx) :!! local) : stack) ctx
    SetLocal :: (KnownNat local, MatchStack '[(GetLocals ctx) :!! local] stack ~ True) =>
        Proxy local ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[(GetLocals ctx) :!! local] stack '[]) ctx
    TeeLocal :: (KnownNat local, MatchStack '[(GetLocals ctx) :!! local] stack ~ True) =>
        Proxy local ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[(GetLocals ctx) :!! local] stack '[(GetLocals ctx) :!! local]) ctx
    GetGlobal :: (KnownNat global) =>
        Proxy global ->
        InstrSeq stack ctx ->
        InstrSeq ((GetGlobalType ((GetGlobals ctx) :!! global)) : stack) ctx
    SetGlobal :: (
            KnownNat global,
            MatchStack '[GetGlobalType ((GetGlobals ctx) :!! global)] stack ~ True,
            IsMutable ((GetGlobals ctx) :!! global) ~ True
        ) =>
        Proxy global ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[GetGlobalType ((GetGlobals ctx) :!! global)] stack '[]) ctx
    I32Const :: InstrSeq stack ctx -> InstrSeq (Val I32 : stack) ctx
    I32UnOp :: (MatchStack '[Val I32] stack ~ True) =>
        IUnOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I32]) ctx
    I32BinOp :: (MatchStack '[Val I32, Val I32] stack ~ True) =>
        IBinOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I32] stack '[Val I32]) ctx
    I32RelOp :: (MatchStack '[Val I32, Val I32] stack ~ True) =>
        IRelOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I32] stack '[Val I32]) ctx