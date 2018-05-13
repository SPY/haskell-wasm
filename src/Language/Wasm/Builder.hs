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
import Control.Monad.State (State, execState, get, put, modify)
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

ret :: (Producer expr) => expr -> GenFun ()
ret e = produce e >> return ()

arg :: (Producer expr) => expr -> GenFun ()
arg e = produce e >> return ()

getSize :: ValueType -> BitSize
getSize I32 = BS32
getSize I64 = BS32
getSize F32 = BS64
getSize F64 = BS64

binOp :: (Producer a, Producer b, OutType a ~ OutType b) => IBinOp -> a -> b -> GenFun (OutType a)
binOp op a b = produce a >> after [IBinOp (getSize $ asValueType a) op] (produce b)

add :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (OutType a)
add = binOp IAdd

and :: (Producer a, Producer b, OutType a ~ OutType b) => a -> b -> GenFun (OutType a)
and = binOp IAnd

i32const :: (Integral i) => i -> GenFun (Proxy I32)
i32const i = appendExpr [I32Const $ asWord32 $ fromIntegral i] >> return Proxy

call :: Proxy t -> Natural -> [GenFun a] -> GenFun (Proxy t)
call t idx args = sequence_ args >> appendExpr [Call idx] >> return t

-- if' :: (Producer pred, OutType pred ~ I32, OutType true ~ OutType false) => pred -> true -> false -> GenFun (OutType true)
-- if' pred true false = do
--     appendExpr [If idx]

class Consumer loc where
    (.=) :: (Producer expr) => loc -> expr -> GenFun ()

instance Consumer (Loc t) where
    (.=) (Loc i) expr = produce expr >> appendExpr [SetLocal i]

instance Consumer (Glob t) where
    (.=) (Glob i) expr = produce expr >> appendExpr [SetGlobal i]

fun :: (Natural -> GenFun a) -> GenMod Natural
fun generator = do
    st@GenModState { target = m@Module { types, functions }, funcIdx } <- get
    let FuncDef { args, results, locals, instrs } = execState (generator funcIdx) $ FuncDef [] [] [] []
    let t = FuncType args results
    let (idx, inserted) = Maybe.fromMaybe (length types, types ++ [t]) $ (\i -> (i, types)) <$> List.findIndex (== t) types
    put $ st {
        target = m { functions = functions ++ [Function (fromIntegral idx) locals instrs], types = inserted },
        funcIdx = funcIdx + 1
    }
    return funcIdx

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
    st@GenModState { target = m@Module { globals }, globIdx } <- get
    put $ st {
        target = m { globals = globals ++ [Global (mkType $ getValueType t) (initWith t val)] },
        globIdx = globIdx + 1
    }
    return $ Glob globIdx

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

    stackStart <- global Const i32 0
    stackEnd <- global Const i32 0
    stackBase <- global Mut i32 0
    stackTop <- global Mut i32 0

    retReg <- global Mut i32 0
    tmpReg <- global Mut i32 0

    heapStart <- global Mut i32 0
    heapNext <- global Mut i32 0
    heapEnd <- global Mut i32 0
    aligned <- fun $ \_ -> do
        size <- param i32
        (size `add` i32const 3) `and` i32const 0xFFFFFFFC
    alloc <- fun $ \self -> do
        size <- param i32
        alignedSize <- local i32
        addr <- local i32
        alignedSize .= call i32 aligned [arg size]
        -- if' ((heapNext `plus` alignedSize) `lt_u` heapEnd)
        --     (do
        --         addr .= nextHeap
        --         nextHeap .= nextHeap `plus` alignedSize
        --         ret addr
        --     )
        --     (do
        --         call gc []
        --         call alloc [ref size]
        --     )
        ret addr
    return ()

{-
    (func $alloc (param $size i32) (result i32)
        (local $aligned-size i32)
        (local $addr i32)
        (set_local $aligned-size (call $alligned (get_local $size)))
        (if (i32.lt_u (i32.add (get_global $heap-next) (get_local $aligned-size)) (get_global $heap-end))
            (then
                (set_local $addr (get_global $heap-next))
                (set_global $heap-next (i32.add (get_global $heap-next) (get_local $aligned-size)))
                (get_local $addr)
            )
            (else
                (call $run-gc)
                (call $alloc (get_local $size))
            )
        )
    )
-}