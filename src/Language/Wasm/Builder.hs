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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Wasm.Builder (
    GenMod,
    genMod,
    global, typedef, fun, funRec, table, memory, dataSegment,
    importFunction, importGlobal, importMemory, importTable,
    export,
    nextFuncIndex, setGlobalInitializer,
    GenFun,
    Glob, Loc, Fn(..), Mem, Tbl,
    param, local, label,
    ret,
    arg,
    i32, i64, f32, f64,
    i32c, i64c, f32c, f64c,
    add, {-inc,-} sub, {-dec,-} mul, div_u, div_s, rem_u, rem_s, and, or, xor, shl, shr_u, shr_s, rotl, rotr,
    eq, ne, lt_s, lt_u, gt_s, gt_u, le_s, le_u, ge_s, ge_u,
    eqz,
    extend_s, extend_u, wrap,
    load, load8_u, load8_s, load16_u, load16_s, load32_u, load32_s,
    store, store8, store16, store32,
    nop,
    call, finish,
    if', loop, block, when, for, while,
    trap, unreachable,
    appendExpr, after,
    Producer, OutType, produce, Consumer, (.=)
) where

import Prelude hiding (and, or)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad.State (State, execState, get, gets, put, modify)
import Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
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

newtype GenFun a = GenFun { unGenFun :: ReaderT Natural (State FuncDef) a } deriving (Functor, Applicative, Monad)

newtype Loc t = Loc Natural deriving (Show, Eq)

class (Monad m) => GenFunMonad m where
    appendExpr :: Expression -> m ()
    inner :: m a -> m Expression
    param :: (ValueTypeable t) => Proxy t -> m (Loc t)
    local :: (ValueTypeable t) => Proxy t -> m (Loc t)
    deep :: m Natural

instance GenFunMonad GenFun where
    appendExpr expr = do
        GenFun $ modify $ \def -> def { instrs = instrs def ++ expr }
        return ()

    inner (GenFun subExpr) = GenFun $ do
        stateBefore <- get
        res <- withReaderT (+1) $ do
            subExpr
            gets instrs
        put stateBefore
        return res

    param t = GenFun $ do
        f@FuncDef { args } <- get
        put $ f { args = args ++ [getValueType t] }
        return $ Loc $ fromIntegral $ length args

    local t = GenFun $ do
        f@FuncDef { args, locals } <- get
        put $ f { locals = locals ++ [getValueType t]}
        return $ Loc $ fromIntegral $ length args + length locals
    
    deep = GenFun ask
-- type GenFun = ReaderT Natural (State FuncDef)

-- genExpr :: Natural -> GenFun a -> Expression
-- genExpr deep gen = instrs $ flip execState (FuncDef [] [] [] []) $ runReaderT gen deep

-- param :: (ValueTypeable t) => Proxy t -> GenFun (Loc t)
-- param t = do
--     f@FuncDef { args } <- get
--     put $ f { args = args ++ [getValueType t] }
--     return $ Loc $ fromIntegral $ length args

-- local :: (ValueTypeable t) => Proxy t -> GenFun (Loc t)
-- local t = do
--     f@FuncDef { args, locals } <- get
--     put $ f { locals = locals ++ [getValueType t]}
--     return $ Loc $ fromIntegral $ length args + length locals

-- appendExpr :: Expression -> GenFun ()
-- appendExpr expr = do
--     modify $ \def -> def { instrs = instrs def ++ expr }
--     return ()

-- after :: Expression -> GenFun a -> GenFun a
-- after instr expr = do
--     res <- expr
--     modify $ \def -> def { instrs = instrs def ++ instr }
--     return res

after :: (GenFunMonad m) => Expression -> m a -> m a
after instr expr = do
    res <- expr
    appendExpr instr
    return res

data TypedExpr m
    = ExprI32 (m (Proxy I32))
    | ExprI64 (m (Proxy I64))
    | ExprF32 (m (Proxy F32))
    | ExprF64 (m (Proxy F64))

data ProducerType
    = LocProd
    | GlobProd
    | ExprProd

type family GetProdType a :: ProducerType where
    GetProdType (Loc t) = 'LocProd
    GetProdType (Glob t) = 'GlobProd
    GetProdType a = 'ExprProd

class (GenFunMonad m) => ProducerHelp (prodType :: ProducerType) m expr where
    type OutTypeHelp prodType expr
    asTypedExprHelp :: expr -> TypedExpr m
    produceHelp :: expr -> m (OutTypeHelp prodType expr)

instance (GenFunMonad m, ValueTypeable t) => ProducerHelp 'LocProd m (Loc t) where
    type OutTypeHelp 'LocProd (Loc t) = Proxy t
    asTypedExprHelp e = case getValueType (t e) of
        I32 -> ExprI32 (produceHelp @'LocProd e >> return Proxy)
        I64 -> ExprI64 (produceHelp @'LocProd e >> return Proxy)
        F32 -> ExprF32 (produceHelp @'LocProd e >> return Proxy)
        F64 -> ExprF64 (produceHelp @'LocProd e >> return Proxy)
        where
            t :: Loc t -> Proxy t
            t _ = Proxy
    produceHelp (Loc i) = appendExpr [GetLocal i] >> return Proxy

instance (GenFunMonad m, ValueTypeable t) => ProducerHelp 'GlobProd m (Glob t) where
    type OutTypeHelp 'GlobProd (Glob t) = Proxy t
    asTypedExprHelp e = case getValueType (t e) of
        I32 -> ExprI32 (produceHelp @'GlobProd e >> return Proxy)
        I64 -> ExprI64 (produceHelp @'GlobProd e >> return Proxy)
        F32 -> ExprF32 (produceHelp @'GlobProd e >> return Proxy)
        F64 -> ExprF64 (produceHelp @'GlobProd e >> return Proxy)
        where
            t :: Glob t -> Proxy t
            t _ = Proxy
    produceHelp (Glob i) = appendExpr [GetGlobal i] >> return Proxy

instance (GenFunMonad m, ValueTypeable t) => ProducerHelp 'ExprProd m (m (Proxy t)) where
    type OutTypeHelp 'ExprProd (m (Proxy t)) = Proxy t
    asTypedExprHelp e = case getValueType (t e) of
        I32 -> ExprI32 (produceHelp @'ExprProd e >> return Proxy)
        I64 -> ExprI64 (produceHelp @'ExprProd e >> return Proxy)
        F32 -> ExprF32 (produceHelp @'ExprProd e >> return Proxy)
        F64 -> ExprF64 (produceHelp @'ExprProd e >> return Proxy)
        where
            t :: (GenFunMonad m) => m (Proxy t) -> Proxy t
            t _ = Proxy
    produceHelp = id

class (GenFunMonad m) => Producer m expr where
    type OutType expr
    asTypedExpr :: expr -> TypedExpr m
    produce :: expr -> m (OutType expr)

instance (GenFunMonad m, ProducerHelp (GetProdType (m a)) m (m a)) => Producer m (m a) where
    type OutType (m a) = OutTypeHelp (GetProdType (m a)) (m a)
    asTypedExpr = asTypedExprHelp @(GetProdType (m a))
    produce = produceHelp @(GetProdType (m a))

ret :: (Producer m expr) => expr -> m (OutType expr)
ret = produce

arg :: (Producer m expr) => expr -> m ()
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

nop :: GenFun ()
nop = appendExpr [Nop]

asValueType :: forall m a . (GenFunMonad m, Producer m a) => a -> ValueType
asValueType a = case asTypedExpr @m a of
    ExprI32 e -> I32
    ExprI64 e -> I64
    ExprF32 e -> F32
    ExprF64 e -> F64

iBinOp :: forall m a b . (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => IBinOp -> a -> b -> m (OutType a)
iBinOp op a b = produce a >> after [IBinOp (getSize $ asValueType @m a) op] (produce b)

add :: forall m a b . (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b) => a -> b -> m (OutType a)
add a b = do
    produce a
    case asValueType @m a of
        I32 -> after [IBinOp BS32 IAdd] (produce b)
        I64 -> after [IBinOp BS64 IAdd] (produce b)
        F32 -> after [FBinOp BS32 FAdd] (produce b)
        F64 -> after [FBinOp BS64 FAdd] (produce b)

-- inc :: (GenFunMonad m, Consumer m a, Producer m a, Integral i) => i -> a -> m ()
-- inc i a = case asTypedExpr a of
--     ExprI32 e -> a .= (e `add` i32c i)
--     ExprI64 e -> a .= (e `add` i64c i)
--     ExprF32 e -> a .= (e `add` f32c (fromIntegral i))
--     ExprF64 e -> a .= (e `add` f64c (fromIntegral i))

sub :: forall m a b . (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b) => a -> b -> m (OutType a)
sub a b = do
    produce a
    case asValueType @m a of
        I32 -> after [IBinOp BS32 ISub] (produce b)
        I64 -> after [IBinOp BS64 ISub] (produce b)
        F32 -> after [FBinOp BS32 FSub] (produce b)
        F64 -> after [FBinOp BS64 FSub] (produce b)

-- dec :: (GenFunMonad m, Consumer m a, Producer m a, Integral i) => i -> a -> m ()
-- dec i a = case asTypedExpr a of
--     ExprI32 e -> a .= (e `sub` i32c i)
--     ExprI64 e -> a .= (e `sub` i64c i)
--     ExprF32 e -> a .= (e `sub` f32c (fromIntegral i))
--     ExprF64 e -> a .= (e `sub` f64c (fromIntegral i))

mul :: forall m a b . (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b) => a -> b -> m (OutType a)
mul a b = do
    produce a
    case asValueType @m a of
        I32 -> after [IBinOp BS32 IMul] (produce b)
        I64 -> after [IBinOp BS64 IMul] (produce b)
        F32 -> after [FBinOp BS32 FMul] (produce b)
        F64 -> after [FBinOp BS64 FMul] (produce b)

div_u :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
div_u = iBinOp IDivU

div_s :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
div_s = iBinOp IDivS

rem_u :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
rem_u = iBinOp IRemU

rem_s :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
rem_s = iBinOp IRemS

and :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
and = iBinOp IAnd

or :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
or = iBinOp IOr

xor :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
xor = iBinOp IXor

shl :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
shl = iBinOp IShl

shr_u :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
shr_u = iBinOp IShrU

shr_s :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
shr_s = iBinOp IShrS

rotl :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
rotl = iBinOp IRotl

rotr :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (OutType a)
rotr = iBinOp IRotr 

relOp :: forall m a b . (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b) => IRelOp -> a -> b -> m (Proxy I32)
relOp op a b = do
    produce a
    produce b
    appendExpr [IRelOp (getSize $ asValueType @m a) op]
    return Proxy

eq :: forall m a b . (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b) => a -> b -> m (Proxy I32)
eq a b = do
    produce a
    produce b
    case asValueType @m a of
        I32 -> appendExpr [IRelOp BS32 IEq]
        I64 -> appendExpr [IRelOp BS64 IEq]
        F32 -> appendExpr [FRelOp BS32 FEq]
        F64 -> appendExpr [FRelOp BS64 FEq]
    return Proxy

ne :: forall m a b . (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b) => a -> b -> m (Proxy I32)
ne a b = do
    produce a
    produce b
    case asValueType @m a of
        I32 -> appendExpr [IRelOp BS32 INe]
        I64 -> appendExpr [IRelOp BS64 INe]
        F32 -> appendExpr [FRelOp BS32 FNe]
        F64 -> appendExpr [FRelOp BS64 FNe]
    return Proxy

lt_s :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (Proxy I32)
lt_s = relOp ILtS

lt_u :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (Proxy I32)
lt_u = relOp ILtS

gt_s :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (Proxy I32)
gt_s = relOp IGtS

gt_u :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (Proxy I32)
gt_u = relOp IGtU

le_s :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (Proxy I32)
le_s = relOp ILeS

le_u :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (Proxy I32)
le_u = relOp ILeS

ge_s :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (Proxy I32)
ge_s = relOp IGeS

ge_u :: (GenFunMonad m, Producer m a, Producer m b, OutType a ~ OutType b, IsInt (OutType a) ~ True) => a -> b -> m (Proxy I32)
ge_u = relOp IGeU

eqz :: forall m a . (GenFunMonad m, Producer m a, IsInt (OutType a) ~ True) => a -> m (Proxy I32)
eqz a = do
    produce a
    case asValueType @m a of
        I32 -> appendExpr [I32Eqz]
        I64 -> appendExpr [I64Eqz]
        _ -> error "Impossible by type constraint"
    return Proxy

i32c :: (GenFunMonad m, Integral i) => i -> m (Proxy I32)
i32c i = appendExpr [I32Const $ asWord32 $ fromIntegral i] >> return Proxy

i64c :: (GenFunMonad m, Integral i) => i -> m (Proxy I64)
i64c i = appendExpr [I64Const $ asWord64 $ fromIntegral i] >> return Proxy

f32c :: (GenFunMonad m) => Float -> m (Proxy F32)
f32c f = appendExpr [F32Const f] >> return Proxy

f64c :: (GenFunMonad m) => Double -> m (Proxy F64)
f64c d = appendExpr [F64Const d] >> return Proxy

extend_u :: (GenFunMonad m, Producer m i, OutType i ~ Proxy I32) => i -> m (Proxy I64)
extend_u small = do
    produce small
    appendExpr [I64ExtendUI32]
    return Proxy

extend_s :: (GenFunMonad m, Producer m i, OutType i ~ Proxy I32) => i -> m (Proxy I64)
extend_s small = do
    produce small
    appendExpr [I64ExtendUI32]
    return Proxy

wrap :: (GenFunMonad m, Producer m i, OutType i ~ Proxy I64) => i -> m (Proxy I32)
wrap big = do
    produce big
    appendExpr [I32WrapI64]
    return Proxy

load :: (GenFunMonad m, ValueTypeable t, Producer m addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> m (Proxy t)
load t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load $ MemArg (fromIntegral offset) (fromIntegral align)]
        F32 -> appendExpr [F32Load $ MemArg (fromIntegral offset) (fromIntegral align)]
        F64 -> appendExpr [F64Load $ MemArg (fromIntegral offset) (fromIntegral align)]
    return Proxy

load8_u :: (GenFunMonad m, ValueTypeable t, IsInt (Proxy t) ~ True, Producer m addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> m (Proxy t)
load8_u t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load8U $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load8U $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"
    return Proxy

load8_s :: (GenFunMonad m, ValueTypeable t, IsInt (Proxy t) ~ True, Producer m addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> m (Proxy t)
load8_s t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load8S $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load8S $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"
    return Proxy

load16_u :: (GenFunMonad m, ValueTypeable t, IsInt (Proxy t) ~ True, Producer m addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> m (Proxy t)
load16_u t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load16U $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load16U $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"
    return Proxy

load16_s :: (GenFunMonad m, ValueTypeable t, IsInt (Proxy t) ~ True, Producer m addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> m (Proxy t)
load16_s t addr offset align = do
    produce addr
    case getValueType t of
        I32 -> appendExpr [I32Load16S $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Load16S $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"
    return Proxy

load32_u :: (GenFunMonad m, ValueTypeable t, IsInt (Proxy t) ~ True, Producer m addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> m (Proxy t)
load32_u t addr offset align = do
    produce addr
    appendExpr [I64Load32U $ MemArg (fromIntegral offset) (fromIntegral align)]
    return Proxy

load32_s :: (GenFunMonad m, ValueTypeable t, IsInt (Proxy t) ~ True, Producer m addr, OutType addr ~ Proxy I32, Integral offset, Integral align)
    => Proxy t
    -> addr
    -> offset
    -> align
    -> m (Proxy t)
load32_s t addr offset align = do
    produce addr
    appendExpr [I64Load32S $ MemArg (fromIntegral offset) (fromIntegral align)]
    return Proxy

store :: forall m addr val offset align . (GenFunMonad m, Producer m addr, OutType addr ~ Proxy I32, Producer m val, Integral offset, Integral align)
    => addr
    -> val
    -> offset
    -> align
    -> m ()
store addr val offset align = do
    produce addr
    produce val
    case asValueType @m val of
        I32 -> appendExpr [I32Store $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Store $ MemArg (fromIntegral offset) (fromIntegral align)]
        F32 -> appendExpr [F32Store $ MemArg (fromIntegral offset) (fromIntegral align)]
        F64 -> appendExpr [F64Store $ MemArg (fromIntegral offset) (fromIntegral align)]

store8 :: forall m addr val offset align . (GenFunMonad m, Producer m addr, OutType addr ~ Proxy I32, Producer m val, IsInt (OutType val) ~ True, Integral offset, Integral align)
    => addr
    -> val
    -> offset
    -> align
    -> m ()
store8 addr val offset align = do
    produce addr
    produce val
    case asValueType @m val of
        I32 -> appendExpr [I32Store8 $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Store8 $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"

store16 :: forall m addr val offset align . (GenFunMonad m, Producer m addr, OutType addr ~ Proxy I32, Producer m val, IsInt (OutType val) ~ True, Integral offset, Integral align)
    => addr
    -> val
    -> offset
    -> align
    -> m ()
store16 addr val offset align = do
    produce addr
    produce val
    case asValueType @m val of
        I32 -> appendExpr [I32Store16 $ MemArg (fromIntegral offset) (fromIntegral align)]
        I64 -> appendExpr [I64Store16 $ MemArg (fromIntegral offset) (fromIntegral align)]
        _ -> error "Impossible by type constraint"

store32 :: (GenFunMonad m, Producer m addr, OutType addr ~ Proxy I32, Producer m val, OutType val ~ Proxy I64, Integral offset, Integral align)
    => addr
    -> val
    -> offset
    -> align
    -> m ()
store32 addr val offset align = do
    produce addr
    produce val
    appendExpr [I64Store32 $ MemArg (fromIntegral offset) (fromIntegral align)]

call :: (GenFunMonad m, Returnable res) => Fn res -> [m a] -> m res
call (Fn idx) args = sequence_ args >> appendExpr [Call idx] >> return returnableValue

br :: (GenFunMonad m) => Label t -> m ()
br (Label labelDeep) = do
    d <- deep
    appendExpr [Br $ d - labelDeep]

finish :: (GenFunMonad m, Producer m val) => val -> m ()
finish val = do
    produce val
    appendExpr [Return]

newtype Label i = Label Natural deriving (Show, Eq)

when :: (GenFunMonad m, Producer m pred, OutType pred ~ Proxy I32)
    => pred
    -> m ()
    -> m ()
when pred body = if' () pred body (return ())

for :: (GenFunMonad m, Producer m pred, OutType pred ~ Proxy I32) => m () -> pred -> m () -> m () -> m ()
for initer pred after body = do
    initer
    let loopBody = do
            body
            after
            loopLabel <- label
            if' () pred (br loopLabel) (return ())
    if' () pred (loop () loopBody) (return ())

while :: (GenFunMonad m, Producer m pred, OutType pred ~ Proxy I32) => pred -> m () -> m ()
while pred body = do
    let loopBody = do
            body
            loopLabel <- label
            if' () pred (br loopLabel) (return ())
    if' () pred (loop () loopBody) (return ())

label :: (GenFunMonad m) => m (Label t)
label = Label <$> deep

if' :: (GenFunMonad m, Producer m pred, OutType pred ~ Proxy I32, Returnable res)
    => res
    -> pred
    -> m res
    -> m res
    -> m res
if' res pred true false = do
    produce pred
    t <- inner true
    f <- inner false
    appendExpr [If (asResultValue res) t f]
    return returnableValue

loop :: (GenFunMonad m, Returnable res) => res -> m res -> m res
loop res body = do
    b <- inner body
    appendExpr [Loop (asResultValue res) b]
    return returnableValue

block :: (GenFunMonad m, Returnable res) => res -> m res -> m res
block res body = do
    b <- inner body
    appendExpr [Block (asResultValue res) b]
    return returnableValue

trap :: (GenFunMonad m) => Proxy t -> m (Proxy t)
trap t = do
    appendExpr [Unreachable]
    return t

unreachable :: (GenFunMonad m) => m ()
unreachable = appendExpr [Unreachable]

class (GenFunMonad m) => Consumer m loc where
    type InputType loc
    infixr 2 .=
    (.=) :: (Producer m expr) => loc -> expr -> m ()

instance (GenFunMonad m) => Consumer m (Loc t) where
    type InputType (Loc t) = Proxy t
    (.=) (Loc i) expr = produce expr >> appendExpr [SetLocal i]

instance (GenFunMonad m) => Consumer m (Glob t) where
    type InputType (Glob t) = Proxy t
    (.=) (Glob i) expr = produce expr >> appendExpr [SetGlobal i]

typedef :: FuncType -> GenMod Natural
typedef t = do
    st@GenModState { target = m@Module { types } } <- get
    let (idx, inserted) = Maybe.fromMaybe (length types, types ++ [t]) $ (\i -> (i, types)) <$> List.findIndex (== t) types
    put $ st { target = m { types = inserted } }
    return $ fromIntegral idx

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
    let GenFun gen = generator (Fn funcIdx)
    let FuncDef { args, locals, instrs } = execState (runReaderT gen 0) $ FuncDef [] [] [] []
    let t = FuncType args (asResultValue res)
    let (idx, inserted) = Maybe.fromMaybe (length types, types ++ [t]) $ (\i -> (i, types)) <$> List.findIndex (== t) types
    put $ st {
        target = m { functions = functions ++ [Function (fromIntegral idx) locals instrs], types = inserted },
        funcIdx = funcIdx + 1
    }
    return $ Fn funcIdx

fun :: (Returnable res) => res -> GenFun res -> GenMod (Fn res)
fun res = funRec res . const

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
        target = m { imports = imports m ++ [Import mod name $ ImportTable $ TableType (Limit min max) AnyFunc] }
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
        target = m { tables = tables m ++ [Table $ TableType (Limit min max) AnyFunc] }
    }
    return $ Tbl 0

dataSegment :: (Integral offset) => offset -> LBS.ByteString -> GenMod ()
dataSegment offset bytes =
    modify $ \(st@GenModState { target = m }) -> st {
        target = m { datas = datas m ++ [DataSegment 0 [I32Const $ fromIntegral offset] bytes] }
    }

asWord32 :: Int32 -> Word32
asWord32 i
    | i >= 0 = fromIntegral i
    | otherwise = 0xFFFFFFFF - (fromIntegral (abs i)) + 1

asWord64 :: Int64 -> Word64
asWord64 i
    | i >= 0 = fromIntegral i
    | otherwise = 0xFFFFFFFFFFFFFFFF - (fromIntegral (abs i)) + 1

rts :: Module
rts = genMod $ do
    gc <- importFunction "rts" "gc" () [I32]
    memory 10 Nothing

    stackStart <- global Const i32 0
    stackEnd <- global Const i32 0
    stackBase <- global Mut i32 0
    stackTop <- global Mut i32 0

    retReg <- global Mut i32 0
    tmpReg <- global Mut i32 0

    heapStart <- global Mut i32 0
    heapNext <- global Mut i32 0
    heapEnd <- global Mut i32 0

    aligned <- fun i32 $ do
        size <- param i32
        (size `add` i32c 3) `and` i32c 0xFFFFFFFC
    alloc <- funRec i32 $ \self -> do
        size <- param i32
        alignedSize <- local i32
        addr <- local i32
        alignedSize .= call aligned [arg size]
        if' i32 ((heapNext `add` alignedSize) `lt_u` heapEnd)
            (do
                addr .= heapNext
                heapNext .= heapNext `add` alignedSize
                ret addr
            )
            (do
                call gc []
                call self [arg size]
            )
    return ()
