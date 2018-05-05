{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}

module Language.Wasm.AST (

) where

import GHC.TypeLits
import Data.Proxy
import Data.Promotion.Prelude.List
import Data.Word (Word32, Word64)

import Language.Wasm.Structure (
        ValueType(..),
        FuncType(..),
        MemArg(..),
        GlobalType(..),
        IUnOp(..),
        IBinOp(..),
        IRelOp(..),
        FUnOp(..),
        FBinOp(..),
        FRelOp(..)
    )

data VType = Val ValueType | Var | Any

type family MatchStack (args :: [VType]) (stack :: [VType]) :: Bool where
    MatchStack (Val v : args) (Val v : stack) = MatchStack args stack
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
    Consume (Val v : args) (Val v : stack) result = Consume args stack result
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

type family GetParams (ft :: FuncType) :: [ValueType] where
    GetParams ('FuncType params results) = params

type family GetResults (ft :: FuncType) :: [ValueType] where
    GetResults ('FuncType params results) = results

data Ctx = Ctx {
    locals :: [VType],
    globals :: [GlobalType],
    labels :: [Maybe ValueType],
    returns :: [ValueType],
    functions :: [FuncType],
    types :: [FuncType]
}

type family GetLocals (ctx :: Ctx) :: [VType] where
    GetLocals ('Ctx locals globals labels returns functions types) = locals

type family GetGlobals (ctx :: Ctx) :: [GlobalType] where
    GetGlobals ('Ctx locals globals labels returns functions types) = globals

type family GetLabels (ctx :: Ctx) :: [Maybe ValueType] where
    GetLabels ('Ctx locals globals labels returns functions types) = labels

type family WithLabel (ctx :: Ctx) (label :: Maybe ValueType) where
    WithLabel ('Ctx locals globals labels returns functions types) label = 'Ctx locals globals (label : labels) returns functions types

type family GetReturns (ctx :: Ctx) :: [ValueType] where
    GetReturns ('Ctx locals globals labels returns functions types) = returns

type family GetFunctions (ctx :: Ctx) :: [FuncType] where
    GetFunctions ('Ctx locals globals labels returns functions types) = functions

type family GetTypes (ctx :: Ctx) :: [FuncType] where
    GetTypes ('Ctx locals globals labels returns functions types) = types

type family GetFTParams (ctx :: Ctx) (function :: Nat) :: [VType] where
    GetFTParams ctx function = AsVType (GetParams ((GetFunctions ctx) :!! function))

type family GetFTResults (ctx :: Ctx) (function :: Nat) :: [VType] where
    GetFTResults ctx function = AsVType (GetResults ((GetFunctions ctx) :!! function))

type family GetTParams (ctx :: Ctx) (typeIdx :: Nat) :: [VType] where
    GetTParams ctx typeIdx = AsVType (GetParams ((GetTypes ctx) :!! typeIdx))

type family GetTResults (ctx :: Ctx) (typeIdx :: Nat) :: [VType] where
    GetTResults ctx typeIdx = AsVType (GetResults ((GetTypes ctx) :!! typeIdx))

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
    Call :: (KnownNat function, MatchStack (GetFTParams ctx function) stack ~ True) =>
        Proxy function ->
        InstrSeq stack ctx ->
        InstrSeq (Consume (GetFTParams ctx function) stack (GetFTResults ctx function)) ctx
    CallIndirect :: (KnownNat typeIdx, MatchStack (GetTParams ctx typeIdx) stack ~ True) =>
        Proxy typeIdx ->
        InstrSeq stack ctx ->
        InstrSeq (Consume (GetTParams ctx typeIdx) stack (GetTResults ctx typeIdx)) ctx
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
    I32Load :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I32]) ctx
    I64Load :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I64]) ctx
    F32Load :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val F32]) ctx
    F64Load :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val F64]) ctx
    I32Load8S :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I32]) ctx
    I32Load8U :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I32]) ctx
    I32Load16S :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I32]) ctx
    I32Load16U :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I32]) ctx
    I64Load8S :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I64]) ctx
    I64Load8U :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I64]) ctx
    I64Load16S :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I64]) ctx
    I64Load16U :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I64]) ctx
    I64Load32S :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I64]) ctx
    I64Load32U :: (MatchStack '[Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I64]) ctx
    I32Store :: (MatchStack '[Val I32, Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I32] stack '[]) ctx
    I64Store :: (MatchStack '[Val I32, Val I64] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I64] stack '[]) ctx
    F32Store  :: (MatchStack '[Val I32, Val F32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val F32] stack '[]) ctx
    F64Store  :: (MatchStack '[Val I32, Val F64] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val F64] stack '[]) ctx
    I32Store8  :: (MatchStack '[Val I32, Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I32] stack '[]) ctx
    I32Store16  :: (MatchStack '[Val I32, Val I32] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I32] stack '[]) ctx
    I64Store8 :: (MatchStack '[Val I32, Val I64] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I64] stack '[]) ctx
    I64Store16 :: (MatchStack '[Val I32, Val I64] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I64] stack '[]) ctx
    I64Store32 :: (MatchStack '[Val I32, Val I64] stack ~ True) =>
        MemArg ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I64] stack '[]) ctx
    CurrentMemory :: InstrSeq stack ctx -> InstrSeq (Val I32 : stack) ctx
    GrowMemory :: (MatchStack '[Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I32]) ctx
    I32Const :: Word32 -> InstrSeq stack ctx -> InstrSeq (Val I32 : stack) ctx
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
    I32Eqz :: (MatchStack '[Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I32]) ctx
    I64Const :: Word64 -> InstrSeq stack ctx -> InstrSeq (Val I64 : stack) ctx
    I64UnOp :: (MatchStack '[Val I64] stack ~ True) =>
        IUnOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I64] stack '[Val I64]) ctx
    I64BinOp :: (MatchStack '[Val I64, Val I64] stack ~ True) =>
        IBinOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32, Val I32] stack '[Val I64]) ctx
    I64RelOp :: (MatchStack '[Val I64, Val I64] stack ~ True) =>
        IRelOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I64, Val I64] stack '[Val I32]) ctx
    I64Eqz :: (MatchStack '[Val I64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I64] stack '[Val I32]) ctx
    F32Const :: Float -> InstrSeq stack ctx -> InstrSeq (Val F32 : stack) ctx
    F32UnOp :: (MatchStack '[Val F32] stack ~ True) =>
        FUnOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F32] stack '[Val F32]) ctx
    F32BinOp :: (MatchStack '[Val F32, Val F32] stack ~ True) =>
        FBinOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F32, Val F32] stack '[Val F32]) ctx
    F32RelOp :: (MatchStack '[Val F32, Val F32] stack ~ True) =>
        FRelOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F32, Val F32] stack '[Val I32]) ctx
    F64Const :: Double -> InstrSeq stack ctx -> InstrSeq (Val F64 : stack) ctx
    F64UnOp :: (MatchStack '[Val F64] stack ~ True) =>
        FUnOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F64] stack '[Val F64]) ctx
    F64BinOp :: (MatchStack '[Val F32, Val F32] stack ~ True) =>
        FBinOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F64, Val F64] stack '[Val F64]) ctx
    F64RelOp :: (MatchStack '[Val F64, Val F64] stack ~ True) =>
        FRelOp ->
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F64, Val F64] stack '[Val I32]) ctx
    I32WrapI64 :: (MatchStack '[Val I64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I64] stack '[Val I32]) ctx
    I32TruncF32U :: (MatchStack '[Val F32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F32] stack '[Val I32]) ctx
    I32TruncF64U :: (MatchStack '[Val F64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F64] stack '[Val I32]) ctx
    I64TruncF32U :: (MatchStack '[Val F32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F32] stack '[Val I64]) ctx
    I64TruncF64U :: (MatchStack '[Val F64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F64] stack '[Val I64]) ctx
    I32TruncF32S :: (MatchStack '[Val F32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F32] stack '[Val I32]) ctx
    I32TruncF64S :: (MatchStack '[Val F64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F64] stack '[Val I32]) ctx
    I64TruncF32S :: (MatchStack '[Val F32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F32] stack '[Val I64]) ctx
    I64TruncF64S :: (MatchStack '[Val F64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F64] stack '[Val I64]) ctx
    I64ExtendI32U :: (MatchStack '[Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I64]) ctx
    I64ExtendI32S :: (MatchStack '[Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val I64]) ctx
    F32ConvertI32U :: (MatchStack '[Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val F32]) ctx
    F32ConvertI64U :: (MatchStack '[Val I64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I64] stack '[Val F32]) ctx
    F64ConvertI32U :: (MatchStack '[Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val F64]) ctx
    F64ConvertI64U :: (MatchStack '[Val I64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I64] stack '[Val F64]) ctx
    F32ConvertI32S :: (MatchStack '[Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val F32]) ctx
    F32ConvertI64S :: (MatchStack '[Val I64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I64] stack '[Val F32]) ctx
    F64ConvertI32S :: (MatchStack '[Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val F64]) ctx
    F64ConvertI64S :: (MatchStack '[Val I64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I64] stack '[Val F64]) ctx
    F32DemoteF64 :: (MatchStack '[Val F64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F64] stack '[Val F32]) ctx
    F64PromoteF32 :: (MatchStack '[Val F32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F32] stack '[Val F64]) ctx
    I32ReinterpretF32 :: (MatchStack '[Val F32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F32] stack '[Val I32]) ctx
    I64ReinterpretF64 :: (MatchStack '[Val F64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val F64] stack '[Val I64]) ctx
    F32ReinterpretI32 :: (MatchStack '[Val I32] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I32] stack '[Val F32]) ctx
    F64ReinterpretI64 :: (MatchStack '[Val I64] stack ~ True) =>
        InstrSeq stack ctx ->
        InstrSeq (Consume '[Val I64] stack '[Val F64]) ctx

-- (func (export "fac-rec") (param i64) (result i64)
--     (if (result i64) (i64.eq (get_local 0) (i64.const 0))
--       (then (i64.const 1))
--       (else
--         (i64.mul (get_local 0) (call 0 (i64.sub (get_local 0) (i64.const 1))))
--       )
--     )
-- )

facRec :: InstrSeq '[Val I32] ('Ctx '[Val I32] '[] '[] '[I32] '[('FuncType '[I32] '[I32])] '[])
facRec = define
    & GetLocal idx0
    & I32Const 0
    & I32RelOp IEq
    & (If resI32
        (then'
            & I32Const 1
        )
        (else'
            & GetLocal idx0
            & I32Const 1
            & GetLocal idx0
            & I32BinOp ISub
            & Call idx0
            & I32BinOp IMul
        )
    )
    where
        resI32 = Proxy @('Just I32)
        idx0 = Proxy @0
        define = Empty
        else' = Empty
        then' = Empty
        infixl 1 &
        x & f = f x
