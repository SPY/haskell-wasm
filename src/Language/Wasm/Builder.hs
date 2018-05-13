{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Wasm.Builder (
) where

import Prelude hiding (and)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad.State (State, execState, get, gets, put, modify)
import Numeric.Natural
import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import Data.Proxy

import qualified Data.Text.Lazy as TL

import Language.Wasm.Structure

data FuncDef = FuncDef {
    args :: [ValueType],
    results :: [ValueType],
    locals :: [ValueType],
    instrs :: Expression
} deriving (Show, Eq)

type GenFun = State FuncDef

genExpr :: GenFun a -> Expression
genExpr gen = instrs $ execState gen $ FuncDef [] [] [] []

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

class Producer expr where
    type OutType expr
    asValueType :: expr -> ValueType
    produce :: expr -> GenFun (OutType expr)

instance (ValueTypeable t) => Producer (Loc t) where
    type OutType (Loc t) = Proxy t
    asValueType e = getValueType (t e)
        where
            t :: Loc t -> Proxy t
            t _ = Proxy
    produce (Loc i) = appendExpr [GetLocal i] >> return Proxy

instance (ValueTypeable t) => Producer (Glob t) where
    type OutType (Glob t) = Proxy t
    asValueType e = getValueType (t e)
        where
            t :: Glob t -> Proxy t
            t _ = Proxy
    produce (Glob i) = appendExpr [GetGlobal i] >> return Proxy

instance (ValueTypeable t) => Producer (GenFun (Proxy t)) where
    type OutType (GenFun (Proxy t)) = Proxy t
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
getSize I64 = BS32
getSize F32 = BS64
getSize F64 = BS64

iBinOp :: (Producer a, Producer b, OutType a ~ OutType b) => IBinOp -> a -> b -> GenFun (OutType a)
iBinOp op a b = produce a >> after [IBinOp (getSize $ asValueType a) op] (produce b)

add :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (OutType a)
add a b = do
    produce a
    case asValueType a of
        I32 -> after [IBinOp BS32 IAdd] (produce b)
        I64 -> after [IBinOp BS64 IAdd] (produce b)
        F32 -> after [FBinOp BS32 FAdd] (produce b)
        F64 -> after [FBinOp BS64 FAdd] (produce b)

sub :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (OutType a)
sub a b = do
    produce a
    case asValueType a of
        I32 -> after [IBinOp BS32 ISub] (produce b)
        I64 -> after [IBinOp BS64 ISub] (produce b)
        F32 -> after [FBinOp BS32 FSub] (produce b)
        F64 -> after [FBinOp BS64 FSub] (produce b)

mul :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (OutType a)
mul a b = do
    produce a
    case asValueType a of
        I32 -> after [IBinOp BS32 IMul] (produce b)
        I64 -> after [IBinOp BS64 IMul] (produce b)
        F32 -> after [FBinOp BS32 FMul] (produce b)
        F64 -> after [FBinOp BS64 FMul] (produce b)

and :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (OutType a)
and = iBinOp IAnd

relOp :: (Producer a, Producer b, OutType a ~ OutType b) => IRelOp -> a -> b -> GenFun (Proxy I32)
relOp op a b = do
    produce a
    produce b
    appendExpr [IRelOp (getSize $ asValueType a) op]
    return Proxy

lt_s :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (Proxy I32)
lt_s = relOp ILtS

lt_u :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (Proxy I32)
lt_u = relOp ILtS

eq :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (Proxy I32)
eq = relOp IEq

i32const :: (Integral i) => i -> GenFun (Proxy I32)
i32const i = appendExpr [I32Const $ asWord32 $ fromIntegral i] >> return Proxy

invoke :: Natural -> [GenFun a] -> GenFun ()
invoke idx args = sequence_ args >> appendExpr [Call idx]

call :: Proxy t -> Natural -> [GenFun a] -> GenFun (Proxy t)
call t idx args = sequence_ args >> appendExpr [Call idx] >> return t

ifExpr :: (Producer pred, OutType pred ~ Proxy I32, ValueTypeable t, Producer true, OutType true ~ Proxy t, Producer false, OutType false ~ Proxy t)
    => Proxy t
    -> pred
    -> true
    -> false
    -> GenFun (Proxy t)
ifExpr t pred true false = do
    produce pred
    appendExpr [If [getValueType t] (genExpr $ produce true) (genExpr $ produce false)]
    return Proxy

class Consumer loc where
    (.=) :: (Producer expr) => loc -> expr -> GenFun ()

instance Consumer (Loc t) where
    (.=) (Loc i) expr = produce expr >> appendExpr [SetLocal i]

instance Consumer (Glob t) where
    (.=) (Glob i) expr = produce expr >> appendExpr [SetGlobal i]

funRec :: (Natural -> GenFun a) -> GenMod Natural
funRec generator = do
    st@GenModState { target = m@Module { types, functions }, funcIdx } <- get
    let FuncDef { args, results, locals, instrs } = execState (generator funcIdx) $ FuncDef [] [] [] []
    let t = FuncType args results
    let (idx, inserted) = Maybe.fromMaybe (length types, types ++ [t]) $ (\i -> (i, types)) <$> List.findIndex (== t) types
    put $ st {
        target = m { functions = functions ++ [Function (fromIntegral idx) locals instrs], types = inserted },
        funcIdx = funcIdx + 1
    }
    return funcIdx

fun :: GenFun a -> GenMod Natural
fun = funRec . const

data GenModState = GenModState {
    funcIdx :: Natural,
    globIdx :: Natural,
    target :: Module
} deriving (Show, Eq)

type GenMod = State GenModState

genMod :: GenMod a -> Module
genMod = target . flip execState (GenModState 0 0 emptyModule)

importFunc :: TL.Text -> TL.Text -> FuncType -> GenMod Natural
importFunc mod name t = do
    st@GenModState { target = m@Module { types, imports }, funcIdx } <- get
    let (idx, inserted) = Maybe.fromMaybe (length types, types ++ [t]) $ (\i -> (i, types)) <$> List.findIndex (== t) types
    put $ st {
        target = m { imports = imports ++ [Import mod name $ ImportFunc $ fromIntegral idx], types = inserted },
        funcIdx = funcIdx + 1
    }
    return funcIdx

importGlobal :: (ValueTypeable t) => TL.Text -> TL.Text -> Proxy t -> GenMod Natural
importGlobal mod name t = do
    st@GenModState { target = m@Module { imports }, globIdx } <- get
    put $ st {
        target = m { imports = imports ++ [Import mod name $ ImportGlobal $ Const $ getValueType t] },
        globIdx = globIdx + 1
    }
    return globIdx

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

memory :: Natural -> Maybe Natural -> GenMod ()
memory min max = do
    modify $ \(st@GenModState { target = m }) -> st {
        target = m { mems = mems m ++ [Memory $ Limit min max] }
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
    gc <- importFunc "rts" "gc" (FuncType [I32] [])
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

    aligned <- fun $ do
        size <- param i32
        (size `add` i32const 3) `and` i32const 0xFFFFFFFC
    alloc <- funRec $ \self -> do
        size <- param i32
        alignedSize <- local i32
        addr <- local i32
        alignedSize .= call i32 aligned [arg size]
        ifExpr i32 ((heapNext `add` alignedSize) `lt_u` heapEnd)
            (do
                addr .= heapNext
                heapNext .= (heapNext `add` alignedSize)
                ret addr
            )
            (do
                invoke gc []
                call i32 self [arg size]
            )
    return ()
