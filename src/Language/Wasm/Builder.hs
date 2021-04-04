{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Wasm.Builder (
    GenMod,
    genMod,
    global, typedef, fun, funRec, declare, implement, table, memory, dataSegment,
    importFunction, importGlobal, importMemory, importTable,
    export,
    nextFuncIndex, setGlobalInitializer,
    GenFun,
    Glob, Loc, Fn(..), Mem, Tbl, Label,
    param, local, label,
    ret,
    arg,
    i32, i64, f32, f64,
    i32c, i64c, f32c, f64c,
    add, inc, sub, dec, mul, div_u, div_s, rem_u, rem_s, and, or, xor, shl, shr_u, shr_s, rotl, rotr,
    clz, ctz, popcnt,
    eq, ne, lt_s, lt_u, gt_s, gt_u, le_s, le_u, ge_s, ge_u,
    eqz,
    div_f, min_f, max_f, copySign,
    abs_f, neg_f, ceil_f, floor_f, trunc_f, nearest_f, sqrt_f,
    lt_f, gt_f, le_f, ge_f,
    wrap, trunc_s, trunc_u, extend_s, extend_u, convert_s, convert_u, demote, promote, reinterpret,
    load, load8_u, load8_s, load16_u, load16_s, load32_u, load32_s,
    store, store8, store16, store32,
    memorySize, growMemory,
    nop, Language.Wasm.Builder.drop, select,
    call, callIndirect, finish, br, brIf, brTable,
    {-if', loop, block, when, for, while,-}
    trap, unreachable,
    appendExpr, after,
    Producer, OutType, produce, Consumer, (.=)
) where

import Prelude hiding (and, or)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad.State (State, execState, get, gets, put, modify)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Numeric.Natural
import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import Data.Proxy

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as LBS

import Language.Wasm.Structure

data FuncDef = FuncDef {
    args :: [ValueType],
    returns :: [ValueType],
    locals :: [ValueType],
    instrs :: Expression
} deriving (Show, Eq)

type GenFun = ReaderT Natural (State FuncDef)

genExpr :: Natural -> GenFun a -> Expression
genExpr deep gen = instrs $ flip execState (FuncDef [] [] [] []) $ runReaderT gen deep

newtype Loc t = Loc Natural deriving (Show, Eq)

param :: (ValueTypeable t) => Proxy t -> GenFun (Loc t)
param t = do
    f@FuncDef { args } <- get
    put $ f { args = args ++ [getValueType t] }
    return $ Loc $ fromIntegral $ length args

local :: (ValueTypeable t) => Proxy t -> GenFun (Loc t)
local t = do
    f@FuncDef { args, locals } <- get
    put $ f { locals = locals ++ [getValueType t]}
    return $ Loc $ fromIntegral $ length args + length locals

appendExpr :: Expression -> GenFun ()
appendExpr expr = do
    modify $ \def -> def { instrs = instrs def ++ expr }
    return ()

after :: Expression -> GenFun a -> GenFun a
after instr expr = do
    res <- expr
    modify $ \def -> def { instrs = instrs def ++ instr }
    return res

data TypedExpr
    = ExprI32 (GenFun (Proxy I32))
    | ExprI64 (GenFun (Proxy I64))
    | ExprF32 (GenFun (Proxy F32))
    | ExprF64 (GenFun (Proxy F64))

class Producer expr where
    type OutType expr
    asTypedExpr :: expr -> TypedExpr
    asValueType :: expr -> ValueType
    produce :: expr -> GenFun (OutType expr)

instance (ValueTypeable t) => Producer (Loc t) where
    type OutType (Loc t) = Proxy t
    asTypedExpr e = case getValueType (t e) of
        I32 -> ExprI32 (produce e >> return Proxy)
        I64 -> ExprI64 (produce e >> return Proxy)
        F32 -> ExprF32 (produce e >> return Proxy)
        F64 -> ExprF64 (produce e >> return Proxy)
        where
            t :: Loc t -> Proxy t
            t _ = Proxy
    asValueType e = getValueType (t e)
        where
            t :: Loc t -> Proxy t
            t _ = Proxy
    produce (Loc i) = appendExpr [GetLocal i] >> return Proxy

instance (ValueTypeable t) => Producer (Glob t) where
    type OutType (Glob t) = Proxy t
    asTypedExpr e = case getValueType (t e) of
        I32 -> ExprI32 (produce e >> return Proxy)
        I64 -> ExprI64 (produce e >> return Proxy)
        F32 -> ExprF32 (produce e >> return Proxy)
        F64 -> ExprF64 (produce e >> return Proxy)
        where
            t :: Glob t -> Proxy t
            t _ = Proxy
    asValueType e = getValueType (t e)
        where
            t :: Glob t -> Proxy t
            t _ = Proxy
    produce (Glob i) = appendExpr [GetGlobal i] >> return Proxy

instance (ValueTypeable t) => Producer (GenFun (Proxy t)) where
    type OutType (GenFun (Proxy t)) = Proxy t
    asTypedExpr e = case getValueType (t e) of
        I32 -> ExprI32 (produce e >> return Proxy)
        I64 -> ExprI64 (produce e >> return Proxy)
        F32 -> ExprF32 (produce e >> return Proxy)
        F64 -> ExprF64 (produce e >> return Proxy)
        where
            t :: GenFun (Proxy t) -> Proxy t
            t _ = Proxy
    asValueType e = getValueType (t e)
        where
            t :: GenFun (Proxy t) -> Proxy t
            t _ = Proxy
    produce = id

ret :: (Producer expr) => expr -> GenFun (OutType expr)
ret = produce

arg :: (Producer expr) => expr -> GenFun ()
arg e = produce e >> return ()

getSize :: ValueType -> BitSize
getSize I32 = BS32
getSize I64 = BS64
getSize F32 = BS32
getSize F64 = BS64

type family IsInt i :: Bool where
    IsInt (Proxy I32) = True
    IsInt (Proxy I64) = True
    IsInt any         = False

type family IsFloat i :: Bool where
    IsFloat (Proxy F32) = True
    IsFloat (Proxy F64) = True
    IsFloat any         = False

nop :: GenFun ()
nop = appendExpr [Nop]

drop :: (Producer val) => val -> GenFun ()
drop val = do
    produce val
    appendExpr [Drop]

select :: (Producer a, Producer b, OutType a ~ OutType b, Producer pred, OutType pred ~ Proxy I32) => pred -> a -> b -> GenFun (OutType a)
select pred a b = select' (produce pred) (produce a) (produce b)
    where
        select' :: GenFun pred -> GenFun val -> GenFun val -> GenFun val
        select' pred a b = do
            a
            res <- b
            pred
            appendExpr [Select]
            return res

iBinOp :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => IBinOp -> a -> b -> GenFun (OutType a)
iBinOp op a b = produce a >> after [IBinOp (getSize $ asValueType a) op] (produce b)

iUnOp :: (Producer a, IsInt (OutType a) ~ True) => IUnOp -> a -> GenFun (OutType a)
iUnOp op a = after [IUnOp (getSize $ asValueType a) op] (produce a)

iRelOp :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => IRelOp -> a -> b -> GenFun (Proxy I32)
iRelOp op a b = do
    produce a
    produce b
    appendExpr [IRelOp (getSize $ asValueType a) op]
    return Proxy

add :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (OutType a)
add a b = do
    produce a
    case asValueType a of
        I32 -> after [IBinOp BS32 IAdd] (produce b)
        I64 -> after [IBinOp BS64 IAdd] (produce b)
        F32 -> after [FBinOp BS32 FAdd] (produce b)
        F64 -> after [FBinOp BS64 FAdd] (produce b)

inc :: (Consumer a, Producer a, Integral i) => i -> a -> GenFun ()
inc i a = case asTypedExpr a of
    ExprI32 e -> a .= (e `add` i32c i)
    ExprI64 e -> a .= (e `add` i64c i)
    ExprF32 e -> a .= (e `add` f32c (fromIntegral i))
    ExprF64 e -> a .= (e `add` f64c (fromIntegral i))

sub :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (OutType a)
sub a b = do
    produce a
    case asValueType a of
        I32 -> after [IBinOp BS32 ISub] (produce b)
        I64 -> after [IBinOp BS64 ISub] (produce b)
        F32 -> after [FBinOp BS32 FSub] (produce b)
        F64 -> after [FBinOp BS64 FSub] (produce b)

dec :: (Consumer a, Producer a, Integral i) => i -> a -> GenFun ()
dec i a = case asTypedExpr a of
    ExprI32 e -> a .= (e `sub` i32c i)
    ExprI64 e -> a .= (e `sub` i64c i)
    ExprF32 e -> a .= (e `sub` f32c (fromIntegral i))
    ExprF64 e -> a .= (e `sub` f64c (fromIntegral i))

mul :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (OutType a)
mul a b = do
    produce a
    case asValueType a of
        I32 -> after [IBinOp BS32 IMul] (produce b)
        I64 -> after [IBinOp BS64 IMul] (produce b)
        F32 -> after [FBinOp BS32 FMul] (produce b)
        F64 -> after [FBinOp BS64 FMul] (produce b)

div_u :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
div_u = iBinOp IDivU

div_s :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
div_s = iBinOp IDivS

rem_u :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
rem_u = iBinOp IRemU

rem_s :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
rem_s = iBinOp IRemS

and :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
and = iBinOp IAnd

or :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
or = iBinOp IOr

xor :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
xor = iBinOp IXor

shl :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
shl = iBinOp IShl

shr_u :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
shr_u = iBinOp IShrU

shr_s :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
shr_s = iBinOp IShrS

rotl :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
rotl = iBinOp IRotl

rotr :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (OutType a)
rotr = iBinOp IRotr 

clz :: (Producer a, IsInt (OutType a) ~ True) => a -> GenFun (OutType a)
clz = iUnOp IClz

ctz :: (Producer a, IsInt (OutType a) ~ True) => a -> GenFun (OutType a)
ctz = iUnOp ICtz

popcnt :: (Producer a, IsInt (OutType a) ~ True) => a -> GenFun (OutType a)
popcnt = iUnOp IPopcnt

eq :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (Proxy I32)
eq a b = do
    produce a
    produce b
    case asValueType a of
        I32 -> appendExpr [IRelOp BS32 IEq]
        I64 -> appendExpr [IRelOp BS64 IEq]
        F32 -> appendExpr [FRelOp BS32 FEq]
        F64 -> appendExpr [FRelOp BS64 FEq]
    return Proxy

ne :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (Proxy I32)
ne a b = do
    produce a
    produce b
    case asValueType a of
        I32 -> appendExpr [IRelOp BS32 INe]
        I64 -> appendExpr [IRelOp BS64 INe]
        F32 -> appendExpr [FRelOp BS32 FNe]
        F64 -> appendExpr [FRelOp BS64 FNe]
    return Proxy

lt_s :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
lt_s = iRelOp ILtS

lt_u :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
lt_u = iRelOp ILtU

gt_s :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
gt_s = iRelOp IGtS

gt_u :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
gt_u = iRelOp IGtU

le_s :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
le_s = iRelOp ILeS

le_u :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
le_u = iRelOp ILeU

ge_s :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
ge_s = iRelOp IGeS

ge_u :: (Producer a, Producer b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
ge_u = iRelOp IGeU

eqz :: (Producer a, IsInt (OutType a) ~ True) => a -> GenFun (Proxy I32)
eqz a = do
    produce a
    case asValueType a of
        I32 -> appendExpr [I32Eqz]
        I64 -> appendExpr [I64Eqz]
        _ -> error "Impossible by type constraint"
    return Proxy

fBinOp :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => FBinOp -> a -> b -> GenFun (OutType a)
fBinOp op a b = produce a >> after [FBinOp (getSize $ asValueType a) op] (produce b)

fUnOp :: (Producer a, IsFloat (OutType a) ~ True) => FUnOp -> a -> GenFun (OutType a)
fUnOp op a = after [FUnOp (getSize $ asValueType a) op] (produce a)

fRelOp :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => FRelOp -> a -> b -> GenFun (Proxy I32)
fRelOp op a b = do
    produce a
    produce b
    appendExpr [FRelOp (getSize $ asValueType a) op]
    return Proxy

div_f :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => a -> b -> GenFun (OutType a)
div_f = fBinOp FDiv

min_f :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => a -> b -> GenFun (OutType a)
min_f = fBinOp FMin

max_f :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => a -> b -> GenFun (OutType a)
max_f = fBinOp FMax

copySign :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => a -> b -> GenFun (OutType a)
copySign = fBinOp FCopySign

abs_f :: (Producer a, IsFloat (OutType a) ~ True) => a -> GenFun (OutType a)
abs_f = fUnOp FAbs

neg_f :: (Producer a, IsFloat (OutType a) ~ True) => a -> GenFun (OutType a)
neg_f = fUnOp FNeg

ceil_f :: (Producer a, IsFloat (OutType a) ~ True) => a -> GenFun (OutType a)
ceil_f = fUnOp FCeil

floor_f :: (Producer a, IsFloat (OutType a) ~ True) => a -> GenFun (OutType a)
floor_f = fUnOp FFloor

trunc_f :: (Producer a, IsFloat (OutType a) ~ True) => a -> GenFun (OutType a)
trunc_f = fUnOp FTrunc

nearest_f :: (Producer a, IsFloat (OutType a) ~ True) => a -> GenFun (OutType a)
nearest_f = fUnOp FAbs

sqrt_f :: (Producer a, IsFloat (OutType a) ~ True) => a -> GenFun (OutType a)
sqrt_f = fUnOp FAbs

lt_f :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
lt_f = fRelOp FLt

gt_f :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
gt_f = fRelOp FGt

le_f :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
le_f = fRelOp FLe

ge_f :: (Producer a, Producer b, OutType a ~ OutType b, IsFloat (OutType a) ~ True) => a -> b -> GenFun (Proxy I32)
ge_f = fRelOp FGe

i32c :: (Integral i) => i -> GenFun (Proxy I32)
i32c i = appendExpr [I32Const $ asWord32 $ fromIntegral i] >> return Proxy

i64c :: (Integral i) => i -> GenFun (Proxy I64)
i64c i = appendExpr [I64Const $ asWord64 $ fromIntegral i] >> return Proxy

f32c :: Float -> GenFun (Proxy F32)
f32c f = appendExpr [F32Const f] >> return Proxy

f64c :: Double -> GenFun (Proxy F64)
f64c d = appendExpr [F64Const d] >> return Proxy

wrap :: (Producer i, OutType i ~ Proxy I64) => i -> GenFun (Proxy I32)
wrap big = do
    produce big
    appendExpr [I32WrapI64]
    return Proxy

trunc_u :: (Producer f, IsFloat (OutType f) ~ True, IsInt (Proxy t) ~ True, ValueTypeable t) => Proxy t -> f -> GenFun (Proxy t)
trunc_u t float = do
    produce float
    appendExpr [ITruncFU (getSize $ getValueType t) (getSize $ asValueType float)]
    return Proxy

trunc_s :: (Producer f, IsFloat (OutType f) ~ True, IsInt (Proxy t) ~ True, ValueTypeable t) => Proxy t -> f -> GenFun (Proxy t)
trunc_s t float = do
    produce float
    appendExpr [ITruncFS (getSize $ getValueType t) (getSize $ asValueType float)]
    return Proxy

extend_u :: (Producer i, OutType i ~ Proxy I32) => i -> GenFun (Proxy I64)
extend_u small = do
    produce small
    appendExpr [I64ExtendUI32]
    return Proxy

extend_s :: (Producer i, OutType i ~ Proxy I32) => i -> GenFun (Proxy I64)
extend_s small = do
    produce small
    appendExpr [I64ExtendSI32]
    return Proxy

convert_u :: (Producer i, IsInt (OutType i) ~ True, IsFloat (Proxy t) ~ True, ValueTypeable t) => Proxy t -> i -> GenFun (Proxy t)
convert_u t int = do
    produce int
    appendExpr [FConvertIU (getSize $ getValueType t) (getSize $ asValueType int)]
    return Proxy

convert_s :: (Producer i, IsInt (OutType i) ~ True, IsFloat (Proxy t) ~ True, ValueTypeable t) => Proxy t -> i -> GenFun (Proxy t)
convert_s t int = do
    produce int
    appendExpr [FConvertIS (getSize $ getValueType t) (getSize $ asValueType int)]
    return Proxy

demote :: (Producer f, OutType f ~ Proxy F64) => f -> GenFun (Proxy F32)
demote f = do
    produce f
    appendExpr [F32DemoteF64]
    return Proxy

promote :: (Producer f, OutType f ~ Proxy F32) => f -> GenFun (Proxy F64)
promote f = do
    produce f
    appendExpr [F64PromoteF32]
    return Proxy

type family SameSize a b where
    SameSize (Proxy I32) (Proxy F32) = True
    SameSize (Proxy I64) (Proxy F64) = True
    SameSize (Proxy F32) (Proxy I32) = True
    SameSize (Proxy F64) (Proxy I64) = True
    SameSize a           b           = False

reinterpret :: (ValueTypeable t, Producer val, SameSize (Proxy t) (OutType val) ~ True) => Proxy t -> val -> GenFun (Proxy t)
reinterpret t val = do
    case (getValueType t, asValueType val) of
        (I32, F32) -> appendExpr [IReinterpretF BS32]
        (I64, F64) -> appendExpr [IReinterpretF BS64]
        (F32, I32) -> appendExpr [FReinterpretI BS32]
        (F64, I64) -> appendExpr [FReinterpretI BS64]
        _ -> error "Impossible by type constraint"
    return Proxy

load :: (ValueTypeable t, Producer addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> GenFun (Proxy t)
load t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load $ MemArg (fromIntegral offset) (fromIntegral align)]
        F32 -> appendExpr [F32Load $ MemArg (fromIntegral offset) (fromIntegral align)]
        F64 -> appendExpr [F64Load $ MemArg (fromIntegral offset) (fromIntegral align)]
    return Proxy

load8_u :: (ValueTypeable t, IsInt (Proxy t) ~ True, Producer addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> GenFun (Proxy t)
load8_u t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load8U $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load8U $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"
    return Proxy

load8_s :: (ValueTypeable t, IsInt (Proxy t) ~ True, Producer addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> GenFun (Proxy t)
load8_s t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load8S $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load8S $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"
    return Proxy

load16_u :: (ValueTypeable t, IsInt (Proxy t) ~ True, Producer addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> GenFun (Proxy t)
load16_u t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load16U $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load16U $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"
    return Proxy

load16_s :: (ValueTypeable t, IsInt (Proxy t) ~ True, Producer addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> GenFun (Proxy t)
load16_s t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load16S $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load16S $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"
    return Proxy

load32_u :: (ValueTypeable t, IsInt (Proxy t) ~ True, Producer addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> GenFun (Proxy t)
load32_u t addr offset align = do
    produce addr
    appendExpr [I64Load32U $ MemArg (fromIntegral offset) (fromIntegral align)]
    return Proxy

load32_s :: (ValueTypeable t, IsInt (Proxy t) ~ True, Producer addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> GenFun (Proxy t)
load32_s t addr offset align = do
    produce addr
    appendExpr [I64Load32S $ MemArg (fromIntegral offset) (fromIntegral align)]
    return Proxy

store :: (Producer addr, OutType addr ~ Proxy I32, Producer val, Integral offset, Integral align)
    => addr
    -> val
    -> offset
    -> align
    -> GenFun ()
store addr val offset align = do
    produce addr
    produce val
    case asValueType val of
        I32 -> appendExpr [I32Store $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Store $ MemArg (fromIntegral offset) (fromIntegral align)]
        F32 -> appendExpr [F32Store $ MemArg (fromIntegral offset) (fromIntegral align)]
        F64 -> appendExpr [F64Store $ MemArg (fromIntegral offset) (fromIntegral align)]

store8 :: (Producer addr, OutType addr ~ Proxy I32, Producer val, IsInt (OutType val) ~ True, Integral offset, Integral align)
    => addr
    -> val
    -> offset
    -> align
    -> GenFun ()
store8 addr val offset align = do
    produce addr
    produce val
    case asValueType val of
        I32 -> appendExpr [I32Store8 $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Store8 $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"

store16 :: (Producer addr, OutType addr ~ Proxy I32, Producer val, IsInt (OutType val) ~ True, Integral offset, Integral align)
    => addr
    -> val
    -> offset
    -> align
    -> GenFun ()
store16 addr val offset align = do
    produce addr
    produce val
    case asValueType val of
        I32 -> appendExpr [I32Store16 $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Store16 $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"

store32 :: (Producer addr, OutType addr ~ Proxy I32, Producer val, OutType val ~ Proxy I64, Integral offset, Integral align)
    => addr
    -> val
    -> offset
    -> align
    -> GenFun ()
store32 addr val offset align = do
    produce addr
    produce val
    appendExpr [I64Store32 $ MemArg (fromIntegral offset) (fromIntegral align)]

memorySize :: GenFun (Proxy I32)
memorySize = appendExpr [CurrentMemory] >> return Proxy

growMemory :: (Producer size, OutType size ~ Proxy I32) => size -> GenFun ()
growMemory size = produce size >> appendExpr [GrowMemory]

call :: (Returnable res) => Fn res -> [GenFun a] -> GenFun res
call (Fn idx) args = sequence_ args >> appendExpr [Call idx] >> return returnableValue

callIndirect :: (Producer index, OutType index ~ Proxy I32, Returnable res) => TypeDef res -> index -> [GenFun a] -> GenFun res
callIndirect (TypeDef idx) index args = do
    sequence_ args
    produce index
    appendExpr [CallIndirect idx]
    return returnableValue

br :: Label t -> GenFun ()
br (Label labelDeep) = do
    deep <- ask
    appendExpr [Br $ deep - labelDeep]

brIf :: (Producer pred, OutType pred ~ Proxy I32) => pred -> Label t -> GenFun ()
brIf pred (Label labelDeep) = do
    produce pred
    deep <- ask
    appendExpr [BrIf $ deep - labelDeep]

brTable :: (Producer selector, OutType selector ~ Proxy I32) => selector -> [Label t] -> Label t -> GenFun ()
brTable selector labels (Label labelDeep) = do
    produce selector
    deep <- ask
    appendExpr [BrTable (map (\(Label d) -> deep - d) labels) $ deep - labelDeep]

finish :: (Producer val) => val -> GenFun ()
finish val = do
    produce val
    appendExpr [Return]

newtype Label i = Label Natural deriving (Show, Eq)
{-
when :: (Producer pred, OutType pred ~ Proxy I32)
    => pred
    -> GenFun ()
    -> GenFun ()
when pred body = if' () pred body (return ())

for :: (Producer pred, OutType pred ~ Proxy I32) => GenFun () -> pred -> GenFun () -> GenFun () -> GenFun ()
for initer pred after body = do
    initer
    let loopBody = do
            body
            after
            loopLabel <- label
            if' () pred (br loopLabel) (return ())
    if' () pred (loop () loopBody) (return ())

while :: (Producer pred, OutType pred ~ Proxy I32) => pred -> GenFun () -> GenFun ()
while pred body = do
    let loopBody = do
            body
            loopLabel <- label
            if' () pred (br loopLabel) (return ())
    if' () pred (loop () loopBody) (return ())-}

label :: GenFun (Label t)
label = Label <$> ask

-- if' :: (Producer pred, OutType pred ~ Proxy I32, Returnable res)
--     => res
--     -> pred
--     -> GenFun res
--     -> GenFun res
--     -> GenFun res
-- if' res pred true false = do
--     produce pred
--     deep <- (+1) <$> ask
--     appendExpr [If (asResultValue res) (genExpr deep $ true) (genExpr deep $ false)]
--     return returnableValue

-- loop :: (Returnable res) => res -> GenFun res -> GenFun res
-- loop res body = do
--     deep <- (+1) <$> ask
--     appendExpr [Loop (asResultValue res) (genExpr deep $ body)]
--     return returnableValue

-- block :: (Returnable res) => res -> GenFun res -> GenFun res
-- block res body = do
--     deep <- (+1) <$> ask
--     appendExpr [Block (asResultValue res) (genExpr deep $ body)]
--     return returnableValue

trap :: Proxy t -> GenFun (Proxy t)
trap t = do
    appendExpr [Unreachable]
    return t

unreachable :: GenFun ()
unreachable = appendExpr [Unreachable]

class Consumer loc where
    infixr 2 .=
    (.=) :: (Producer expr) => loc -> expr -> GenFun ()

instance Consumer (Loc t) where
    (.=) (Loc i) expr = produce expr >> appendExpr [SetLocal i]

instance Consumer (Glob t) where
    (.=) (Glob i) expr = produce expr >> appendExpr [SetGlobal i]

newtype TypeDef t = TypeDef Natural deriving (Show, Eq)

typedef :: (Returnable res) => res -> [ValueType] -> GenMod (TypeDef res)
typedef res args = do
    let t = FuncType args (asResultValue res)
    st@GenModState { target = m@Module { types } } <- get
    let (idx, inserted) = Maybe.fromMaybe (length types, types ++ [t]) $ (\i -> (i, types)) <$> List.findIndex (== t) types
    put $ st { target = m { types = inserted } }
    return $ TypeDef $ fromIntegral idx

newtype Fn a = Fn Natural deriving (Show, Eq)

class Returnable a where
    asResultValue :: a -> [ValueType]
    returnableValue :: a

instance (ValueTypeable t) => Returnable (Proxy t) where
    asResultValue t = [getValueType t]
    returnableValue = Proxy

instance Returnable () where
    asResultValue _ = []
    returnableValue = ()

funRec :: (Returnable res) => res -> (Fn res -> GenFun res) -> GenMod (Fn res)
funRec res generator = do
    st@GenModState { target = m@Module { types, functions }, funcIdx } <- get
    let FuncDef { args, locals, instrs } = execState (runReaderT (generator (Fn funcIdx)) 0) $ FuncDef [] [] [] []
    let t = FuncType args (asResultValue res)
    let (idx, inserted) = Maybe.fromMaybe (length types, types ++ [t]) $ (\i -> (i, types)) <$> List.findIndex (== t) types
    put $ st {
        target = m { functions = functions ++ [Function (fromIntegral idx) locals instrs], types = inserted },
        funcIdx = funcIdx + 1
    }
    return $ Fn funcIdx

fun :: (Returnable res) => res -> GenFun res -> GenMod (Fn res)
fun res = funRec res . const

declare :: (Returnable res) => res -> [ValueType] -> GenMod (Fn res)
declare res args = do
    st@GenModState { target = m@Module { types, functions }, funcIdx } <- get
    let t = FuncType args (asResultValue res)
    let (idx, inserted) = Maybe.fromMaybe (length types, types ++ [t]) $ (\i -> (i, types)) <$> List.findIndex (== t) types
    let err = error "Declared function doesn't have implementation"
    put $ st {
        target = m { functions = functions ++ [Function (fromIntegral idx) err err], types = inserted },
        funcIdx = funcIdx + 1
    }
    return $ Fn funcIdx

implement :: (Returnable res) => Fn res -> GenFun res -> GenMod (Fn res)
implement (Fn funcIdx) generator = do
    st@GenModState { target = m@Module { types, functions, imports } } <- get
    let FuncDef { args, locals, instrs } = execState (runReaderT generator 0) $ FuncDef [] [] [] []
    let locIdx = fromIntegral funcIdx - (length $ filter isFuncImport imports)
    let (l, inst : r) = splitAt locIdx functions
    let typeIdx = funcType inst
    let FuncType ps _ = types !! fromIntegral typeIdx
    if args /= ps then error "Arguments list in implementation doesn't match with declared type" else return ()
    put $ st { target = m { functions = l ++ [Function typeIdx locals instrs] ++ r } }
    return $ Fn funcIdx

nextFuncIndex :: GenMod Natural
nextFuncIndex = gets funcIdx

data GenModState = GenModState {
    funcIdx :: Natural,
    globIdx :: Natural,
    target :: Module
} deriving (Show, Eq)

type GenMod = State GenModState

genMod :: GenMod a -> Module
genMod = target . flip execState (GenModState 0 0 emptyModule)

importFunction :: (Returnable res) => TL.Text -> TL.Text -> res -> [ValueType] -> GenMod (Fn res)
importFunction mod name res params = do
    st@GenModState { target = m@Module { types, imports }, funcIdx } <- get
    let t = FuncType params (asResultValue res)
    let (idx, inserted) = Maybe.fromMaybe (length types, types ++ [t]) $ (\i -> (i, types)) <$> List.findIndex (== t) types
    put $ st {
        target = m { imports = imports ++ [Import mod name $ ImportFunc $ fromIntegral idx], types = inserted },
        funcIdx = funcIdx + 1
    }
    return (Fn funcIdx)

importGlobal :: (ValueTypeable t) => TL.Text -> TL.Text -> Proxy t -> GenMod (Glob t)
importGlobal mod name t = do
    st@GenModState { target = m@Module { imports }, globIdx } <- get
    put $ st {
        target = m { imports = imports ++ [Import mod name $ ImportGlobal $ Const $ getValueType t] },
        globIdx = globIdx + 1
    }
    return $ Glob globIdx

importMemory :: TL.Text -> TL.Text -> Natural -> Maybe Natural -> GenMod Mem
importMemory mod name min max = do
    modify $ \(st@GenModState { target = m }) -> st {
        target = m { imports = imports m ++ [Import mod name $ ImportMemory $ Limit min max] }
    }
    return $ Mem 0

importTable :: TL.Text -> TL.Text -> Natural -> Maybe Natural -> GenMod Tbl
importTable mod name min max = do
    modify $ \(st@GenModState { target = m }) -> st {
        target = m { imports = imports m ++ [Import mod name $ ImportTable $ TableType (Limit min max) FuncRef] }
    }
    return $ Tbl 0

class Exportable e where
    type AfterExport e
    export :: TL.Text -> e -> GenMod (AfterExport e)

instance (Exportable e) => Exportable (GenMod e) where
    type AfterExport (GenMod e) = AfterExport e
    export name def = do
        ent <- def
        export name ent

instance Exportable (Fn t) where
    type AfterExport (Fn t) = Fn t
    export name (Fn funIdx) = do
        modify $ \(st@GenModState { target = m }) -> st {
            target = m { exports = exports m ++ [Export name $ ExportFunc funIdx] }
        }
        return (Fn funIdx)

instance Exportable (Glob t) where
    type AfterExport (Glob t) = Glob t
    export name g@(Glob idx) = do
        modify $ \(st@GenModState { target = m }) -> st {
            target = m { exports = exports m ++ [Export name $ ExportGlobal idx] }
        }
        return g

instance Exportable Mem where
    type AfterExport Mem = Mem
    export name (Mem memIdx) = do
        modify $ \(st@GenModState { target = m }) -> st {
            target = m { exports = exports m ++ [Export name $ ExportMemory memIdx] }
        }
        return (Mem memIdx)

instance Exportable Tbl where
    type AfterExport Tbl = Tbl
    export name (Tbl tableIdx) = do
        modify $ \(st@GenModState { target = m }) -> st {
            target = m { exports = exports m ++ [Export name $ ExportTable tableIdx] }
        }
        return (Tbl tableIdx)

class ValueTypeable a where
    type ValType a
    getValueType :: (Proxy a) -> ValueType
    initWith :: (Proxy a) -> (ValType a) -> Expression

instance ValueTypeable I32 where
    type ValType I32 = Word32
    getValueType _ = I32
    initWith _ w = [I32Const w]

instance ValueTypeable I64 where
    type ValType I64 = Word64
    getValueType _ = I64
    initWith _ w = [I64Const w]

instance ValueTypeable F32 where
    type ValType F32 = Float
    getValueType _ = F32
    initWith _ f = [F32Const f]

instance ValueTypeable F64 where
    type ValType F64 = Double
    getValueType _ = F64
    initWith _ d = [F64Const d]

i32 = Proxy @I32
i64 = Proxy @I64
f32 = Proxy @F32
f64 = Proxy @F64

newtype Glob t = Glob Natural deriving (Show, Eq)

global :: (ValueTypeable t) => (ValueType -> GlobalType) -> Proxy t -> (ValType t) -> GenMod (Glob t)
global mkType t val = do
    idx <- gets globIdx
    modify $ \(st@GenModState { target = m }) -> st {
        target = m { globals = globals m ++ [Global (mkType $ getValueType t) (initWith t val)] },
        globIdx = idx + 1
    }
    return $ Glob idx

setGlobalInitializer :: forall t . (ValueTypeable t) => Glob t -> (ValType t) -> GenMod ()
setGlobalInitializer (Glob idx) val = do
    modify $ \(st@GenModState { target = m }) ->
        let globImpsLen = length $ filter isGlobalImport $ imports m in
        let (h, glob:t) = splitAt (fromIntegral idx - globImpsLen) $ globals m in
        st {
            target = m { globals = h ++ [glob { initializer = initWith (Proxy @t) val }] ++ t }
        }

newtype Mem = Mem Natural deriving (Show, Eq)

memory :: Natural -> Maybe Natural -> GenMod Mem
memory min max = do
    modify $ \(st@GenModState { target = m }) -> st {
        target = m { mems = mems m ++ [Memory $ Limit min max] }
    }
    return $ Mem 0

newtype Tbl = Tbl Natural deriving (Show, Eq)

table :: Natural -> Maybe Natural -> GenMod Tbl
table min max = do
    modify $ \(st@GenModState { target = m }) -> st {
        target = m { tables = tables m ++ [Table $ TableType (Limit min max) FuncRef] }
    }
    return $ Tbl 0

dataSegment :: (Producer offset, OutType offset ~ Proxy I32) => offset -> LBS.ByteString -> GenMod ()
dataSegment offset bytes =
    modify $ \(st@GenModState { target = m }) -> st {
        target = m { datas = datas m ++ [DataSegment 0 (genExpr 0 (produce offset)) bytes] }
    }

asWord32 :: Int32 -> Word32
asWord32 i
    | i >= 0 = fromIntegral i
    | otherwise = 0xFFFFFFFF - (fromIntegral (abs i)) + 1

asWord64 :: Int64 -> Word64
asWord64 i
    | i >= 0 = fromIntegral i
    | otherwise = 0xFFFFFFFFFFFFFFFF - (fromIntegral (abs i)) + 1

-- rts :: Module
-- rts = genMod $ do
--     gc <- importFunction "rts" "gc" () [I32]
--     memory 10 Nothing

--     stackStart <- global Const i32 0
--     stackEnd <- global Const i32 0
--     stackBase <- global Mut i32 0
--     stackTop <- global Mut i32 0

--     retReg <- global Mut i32 0
--     tmpReg <- global Mut i32 0

--     heapStart <- global Mut i32 0
--     heapNext <- global Mut i32 0
--     heapEnd <- global Mut i32 0

--     aligned <- fun i32 $ do
--         size <- param i32
--         (size `add` i32c 3) `and` i32c 0xFFFFFFFC
--     alloc <- funRec i32 $ \self -> do
--         size <- param i32
--         alignedSize <- local i32
--         addr <- local i32
--         alignedSize .= call aligned [arg size]
--         if' i32 ((heapNext `add` alignedSize) `lt_u` heapEnd)
--             (do
--                 addr .= heapNext
--                 heapNext .= heapNext `add` alignedSize
--                 ret addr
--             )
--             (do
--                 call gc []
--                 call self [arg size]
--             )
--     return ()
