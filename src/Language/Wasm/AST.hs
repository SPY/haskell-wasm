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
        IUnOp(..),
        IBinOp(..),
        IRelOp(..)
    )

data VType = Val ValueType | Var | Any

type family MatchStack (args :: [ValueType]) (stack :: [ValueType]) :: Bool where
    MatchStack (I32 : args) (I32 : stack) = MatchStack args stack
    MatchStack '[] stack = True
    MatchStack args stack = TypeError (
            Text "Cannot match stack with instruction arguments." :$$:
            Text "Expected arguments: " :<>: ShowType args :$$:
            Text "Actual stack: " :<>: ShowType stack
        )

type family Consume (args :: [ValueType]) (stack :: [ValueType]) (result :: [ValueType]) :: [ValueType] where
    Consume (I32 : args) (I32 : stack) result = Consume args stack result
    Consume '[] stack result = result :++ stack
    Consume args stack result = TypeError (
            Text "Cannot consume stack." :$$:
            Text "Expected arguments: " :<>: ShowType args :$$:
            Text "Actual stack: " :<>: ShowType stack
        )

data InstrSeq (stack :: [ValueType]) (locals :: [ValueType]) where
    Empty :: InstrSeq '[] locals
    I32Const :: InstrSeq stack locals -> InstrSeq (I32 : stack) locals
    I32UnOp :: (MatchStack '[I32] stack ~ True) =>
        IUnOp ->
        InstrSeq stack locals ->
        InstrSeq (Consume '[I32] stack '[I32]) locals
    I32BinOp :: (MatchStack '[I32, I32] stack ~ True) =>
        IBinOp ->
        InstrSeq stack locals ->
        InstrSeq (Consume '[I32, I32] stack '[I32]) locals
    I32RelOp :: (MatchStack '[I32, I32] stack ~ True) =>
        IRelOp ->
        InstrSeq stack locals ->
        InstrSeq (Consume '[I32, I32] stack '[I32]) locals
    GetLocal :: (KnownNat local) =>
        Proxy local ->
        InstrSeq stack locals ->
        InstrSeq ((locals :!! local) : stack) locals
    SetLocal :: (KnownNat local, MatchStack '[locals :!! local] stack ~ True) =>
        Proxy local ->
        InstrSeq stack locals ->
        InstrSeq (Consume '[locals :!! local] stack '[]) locals
    TeeLocal :: (KnownNat local, MatchStack '[locals :!! local] stack ~ True) =>
        Proxy local ->
        InstrSeq stack locals ->
        InstrSeq (Consume '[locals :!! local] stack '[locals :!! local]) locals
    Drop :: InstrSeq (any : stack) locals -> InstrSeq stack locals
