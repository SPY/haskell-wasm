{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Wasm.Interpreter (
    Value(..),
    Store,
    ModuleInstance,
    instantiate,
    invoke,
    invokeExport,
    emptyStore,
    emptyImports
) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as LBS

import Data.Vector (Vector, (!), (!?), (//))
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable.Mutable as IOVector
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32, Int64)
import Numeric.Natural (Natural)
import qualified Control.Monad as Monad
import Data.Monoid ((<>))
import Data.Bits (
        (.|.),
        (.&.),
        xor,
        shiftL,
        shiftR,
        rotateL,
        rotateR,
        popCount,
        countLeadingZeros,
        countTrailingZeros
    )
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

import Debug.Trace as Debug

import Language.Wasm.Structure as Struct

data Value =
    VI32 Word32
    | VI64 Word64
    | VF32 Float
    | VF64 Double
    deriving (Eq, Show)

asInt32 :: Word32 -> Int32
asInt32 w =
    let base = fromIntegral $ w .&. 0x7FFFFFFF in
    let sign = w .&. 0x80000000 in
    if sign /= 0 then -base else base

asInt64 :: Word64 -> Int64
asInt64 w =
    let base = fromIntegral $ w .&. 0x7FFFFFFFFFFFFFFF in
    let sign = w .&. 0x8000000000000000 in
    if sign /= 0 then -base else base

asWord32 :: Int32 -> Word32
asWord32 i
    | i >= 0 = fromIntegral i
    | otherwise = 0x80000000 .|. (fromIntegral (abs i))

asWord64 :: Int64 -> Word64
asWord64 i
    | i >= 0 = fromIntegral i
    | otherwise = 0x8000000000000000 .|. (fromIntegral (abs i))

-- brough from https://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

nearest :: (Floating a, RealFrac a) => a -> a
nearest f
    | f >= 0 && f <= 0.5 = 0
    | f < 0 && f >= -0.5 = -0
    | otherwise =
        let i = floor f :: Int64 in
        let fi = fromIntegral i in
        let r = abs f - abs fi in
        if r == 0.5
        then (
            case (even i, f < 0) of
                (True, _) -> fi
                (_, True) -> fi - 1.0
                (_, False) -> fi + 1.0
        )
        else fromIntegral (round f :: Int64)

data Label = Label ResultType deriving (Show, Eq)

type Address = Int

data TableInstance = TableInstance {
    elements :: Vector (Maybe Address),
    maxLen :: Maybe Int
}

data MemoryInstance = MemoryInstance {
    memory :: IOVector Word8,
    maxLen :: Maybe Int -- in page size (64Ki)
}

data GlobalInstance = GIConst Value | GIMut (IORef Value)

data ExportInstance = ExportInstance TL.Text ExternalValue deriving (Eq, Show)

data ExternalValue =
    ExternFunction Address
    | ExternTable Address
    | ExternMemory Address
    | ExternGlobal Address
    deriving (Eq, Show)

data FunctionInstance =
    FunctionInstance {
        funcType :: FuncType,
        moduleInstance :: ModuleInstance,
        code :: Function
    }
    | HostInstance {
        funcType :: FuncType,
        tag :: TL.Text
    }
    deriving (Show, Eq)

data Store = Store {
    funcInstances :: Vector FunctionInstance,
    tableInstances :: Vector TableInstance,
    memInstances :: Vector MemoryInstance,
    globalInstances :: Vector GlobalInstance
}

emptyStore :: Store
emptyStore = Store {
    funcInstances = Vector.empty,
    tableInstances = Vector.empty,
    memInstances = Vector.empty,
    globalInstances = Vector.empty
}

data ModuleInstance = ModuleInstance {
    funcTypes :: Vector FuncType,
    funcaddrs :: Vector Address,
    tableaddrs :: Vector Address,
    memaddrs :: Vector Address,
    globaladdrs :: Vector Address,
    exports :: Vector ExportInstance
} deriving (Eq, Show)

calcInstance :: Store -> Imports -> Module -> ModuleInstance
calcInstance (Store fs ts ms gs) imps Module {functions, types, tables, mems, globals, exports, imports} =
    let funLen = length fs in
    let tableLen = length ts in
    let memLen = length ms in
    let globalLen = length gs in
    let getImpIdx (Import m n _) =
            case Map.lookup (m, n) imps of
                Just idx -> idx
                Nothing -> error $ "Cannot find import from module " ++ show m ++ " with name " ++ show n
    in
    let funImps = map getImpIdx $ filter isFuncImport imports in
    let tableImps = map getImpIdx $ filter isTableImport imports in
    let memImps = map getImpIdx $ filter isMemImport imports in
    let globalImps = map getImpIdx $ filter isGlobalImport imports in
    let funs = Vector.fromList $ map (\(ExternFunction i) -> i) funImps ++ [funLen..funLen + length functions - 1] in
    let tbls = Vector.fromList $ map (\(ExternTable i) -> i) tableImps ++ [tableLen..tableLen + length tables - 1] in
    let memories = Vector.fromList $ map (\(ExternMemory i) -> i) memImps ++ [memLen..memLen + length mems - 1] in
    let globs = Vector.fromList $ map (\(ExternGlobal i) -> i) globalImps ++ [globalLen..globalLen + length globals - 1] in
    let
        refExport (Export name (ExportFunc idx)) =
            ExportInstance name $ ExternFunction $ funs ! fromIntegral idx
        refExport (Export name (ExportTable idx)) =
            ExportInstance name $ ExternTable $ tbls ! fromIntegral idx
        refExport (Export name (ExportMemory idx)) =
            ExportInstance name $ ExternMemory $ memories ! fromIntegral idx
        refExport (Export name (ExportGlobal idx)) =
            ExportInstance name $ ExternGlobal $ globs ! fromIntegral idx
    in
    ModuleInstance {
        funcTypes = Vector.fromList types,
        funcaddrs = funs,
        tableaddrs = tbls,
        memaddrs = memories,
        globaladdrs = globs,
        exports = Vector.fromList $ map refExport exports
    }

type Imports = Map.Map (TL.Text, TL.Text) ExternalValue

emptyImports :: Imports
emptyImports = Map.empty

allocFunctions :: ModuleInstance -> [Function] -> Vector FunctionInstance
allocFunctions inst@ModuleInstance {funcTypes} funs =
    let mkFuncInst f@Function {funcType} = FunctionInstance (funcTypes ! (fromIntegral funcType)) inst f in
    Vector.fromList $ map mkFuncInst funs

getGlobalValue :: ModuleInstance -> Store -> Natural -> IO Value
getGlobalValue inst store idx =
    let addr = case globaladdrs inst !? fromIntegral idx of
            Just a -> a
            Nothing -> error "Global index is out of range. It can happen if initializer refs non-import global."
    in
    case globalInstances store ! addr of
        GIConst v -> return v
        GIMut ref -> readIORef ref

-- due the validation there can be only these instructions
evalConstExpr :: ModuleInstance -> Store -> [Instruction] -> IO Value
evalConstExpr _ _ [I32Const v] = return $ VI32 v
evalConstExpr _ _ [I64Const v] = return $ VI64 v
evalConstExpr _ _ [F32Const v] = return $ VF32 v
evalConstExpr _ _ [F64Const v] = return $ VF64 v
evalConstExpr inst store [GetGlobal i] = getGlobalValue inst store i
evalConstExpr _ _ instrs = error $ "Global initializer contains unsupported instructions: " ++ show instrs

allocAndInitGlobals :: ModuleInstance -> Store -> [Global] -> IO (Vector GlobalInstance)
allocAndInitGlobals inst store globs = Vector.fromList <$> mapM allocGlob globs
    where
        runIniter :: [Instruction] -> IO Value
        -- the spec says get global can ref only imported globals
        -- only they are in store for this moment
        runIniter = evalConstExpr inst store

        allocGlob :: Global -> IO GlobalInstance
        allocGlob (Global (Const _) initer) = GIConst <$> runIniter initer
        allocGlob (Global (Mut _) initer) = do
            val <- runIniter initer
            GIMut <$> newIORef val

allocTables :: [Table] -> Vector TableInstance
allocTables tables = Vector.fromList $ map allocTable tables
    where
        allocTable :: Table -> TableInstance
        allocTable (Table (TableType (Limit from to) _)) =
            TableInstance {
                elements = Vector.fromList $ replicate (fromIntegral from) Nothing,
                maxLen = fromIntegral <$> to
            }

pageSize :: Int
pageSize = 64 * 1024

allocMems :: [Memory] -> IO (Vector MemoryInstance)
allocMems mems = Vector.fromList <$> mapM allocMem mems
    where
        allocMem :: Memory -> IO MemoryInstance
        allocMem (Memory (Limit from to)) = do
            memory <- IOVector.replicate (fromIntegral from * pageSize) 0
            return $ MemoryInstance {
                memory,
                maxLen = fromIntegral <$> to
            }

initialize :: ModuleInstance -> Module -> Store -> IO Store
initialize inst Module {elems, datas, start} store = do
    storeWithTables <- Monad.foldM initElem store elems
    storeWithMems <- Monad.foldM initData storeWithTables datas
    case start of
        Just (StartFunction idx) -> do
            let funInst = funcInstances store ! (funcaddrs inst ! fromIntegral idx)
            [] <- eval storeWithMems funInst []
            return storeWithMems
        Nothing -> return storeWithMems
    where
        fitOrGrowTable :: Address -> Store -> Int -> TableInstance
        fitOrGrowTable idx st last =
            let t@(TableInstance elems maxLen) = tableInstances st ! idx in
            let len = Vector.length elems in
            let increased = TableInstance (elems Vector.++ (Vector.fromList $ replicate (last - len) Nothing)) maxLen in
            if last < len
            then t
            else case maxLen of
                Nothing -> increased
                Just max ->
                    if max < last
                    then error $ "Max table length reached. Max " ++ show max ++ ", but requested " ++ show last
                    else increased

        initElem :: Store -> ElemSegment -> IO Store
        initElem st ElemSegment {tableIndex, offset, funcIndexes} = do
            VI32 val <- evalConstExpr inst store offset
            let from = fromIntegral val
            let funcs = map ((funcaddrs inst !) . fromIntegral) funcIndexes
            let idx = tableaddrs inst ! fromIntegral tableIndex
            let last = from + length funcs
            let TableInstance elems maxLen = fitOrGrowTable idx st last
            let len = Vector.length elems
            let table = TableInstance (elems // zip [from..] (map Just funcs)) maxLen
            return $ st { tableInstances = tableInstances st Vector.// [(idx, table)] }

        fitOrGrowMemory :: Address -> Store -> Int -> IO MemoryInstance
        fitOrGrowMemory idx st last = do
            let m@(MemoryInstance mem maxLen) = memInstances st ! idx
            let len = IOVector.length mem
            let increased = do
                    let pages = (last - len) `div` pageSize + (if (last - len) `rem` len == 0 then 0 else 1)
                    mem' <- IOVector.grow mem $ pages * pageSize
                    return $ MemoryInstance mem' maxLen
            if last < len
            then return m
            else case maxLen of
                Nothing -> increased
                Just max ->
                    let maxInBytes = max * pageSize in
                    if maxInBytes <= last
                    then error $ "Max memory length reached. Max " ++ show max ++ "(" ++ show maxInBytes ++ "b), but requested " ++ show last
                    else increased

        initData :: Store -> DataSegment -> IO Store
        initData st DataSegment {memIndex, offset, chunk} = do
            VI32 val <- evalConstExpr inst store offset
            let from = fromIntegral val
            let idx = memaddrs inst ! fromIntegral memIndex
            let last = from + (fromIntegral $ LBS.length chunk)
            MemoryInstance mem maxLen <- fitOrGrowMemory idx st last
            mapM_ (\(i,b) -> IOVector.write mem i b) $ zip [from..] $ LBS.unpack chunk
            return $ st { memInstances = memInstances st // [(idx, MemoryInstance mem maxLen)] }

instantiate :: Store -> Imports -> Module -> IO (ModuleInstance, Store)
instantiate st imps m = do
    let inst = calcInstance st imps m
    let functions = funcInstances st <> (allocFunctions inst $ Struct.functions m)
    globals <- (globalInstances st <>) <$> (allocAndInitGlobals inst st $ Struct.globals m)
    let tables = tableInstances st <> (allocTables $ Struct.tables m)
    mems <- (memInstances st <>) <$> (allocMems $ Struct.mems m)
    st' <- initialize inst m $ st {
        funcInstances = functions,
        tableInstances = tables,
        memInstances = mems,
        globalInstances = globals
    }
    return (inst, st')

type Stack = [Value]

data EvalCtx = EvalCtx {
    locals :: Vector Value,
    labels :: [Label],
    stack :: Stack
} deriving (Show, Eq)

data EvalResult =
    Done EvalCtx
    | Break Int [Value] EvalCtx
    | Trap
    | ReturnFn [Value]
    deriving (Show, Eq)

eval :: Store -> FunctionInstance -> [Value] -> IO [Value]
eval store FunctionInstance { funcType, moduleInstance, code = Function { localTypes, body} } args = do
    let checkedArgs = zipWith checkValType (params funcType) args
    let initialContext = EvalCtx {
            locals = Vector.fromList $ checkedArgs ++ map initLocal localTypes,
            labels = [Label $ results funcType],
            stack = []
        }
    res <- go initialContext body
    case res of
        Done ctx -> return $ reverse $ stack ctx
        ReturnFn r -> return r
        Break 0 r _ -> return $ reverse r
        Break _ _ _ -> error "Break is out of range"
        Trap -> error "Evaluation terminated with Trap"
    where
        checkValType :: ValueType -> Value -> Value
        checkValType I32 (VI32 v) = VI32 v
        checkValType I64 (VI64 v) = VI64 v
        checkValType F32 (VF32 v) = VF32 v
        checkValType F64 (VF64 v) = VF64 v
        checkValType _   _        = error "Value types do not match provided value"

        initLocal :: ValueType -> Value
        initLocal I32 = VI32 0
        initLocal I64 = VI64 0
        initLocal F32 = VF32 0
        initLocal F64 = VF64 0

        go :: EvalCtx -> [Instruction] -> IO EvalResult
        go ctx [] = return $ Done ctx
        go ctx (instr:rest) = do
            res <- step ctx instr
            -- case Debug.trace ("instr " ++ show instr ++ " --> " ++ show res) $ res of
            case res of
                Done ctx' -> go ctx' rest
                command -> return command

        step :: EvalCtx -> Instruction -> IO EvalResult
        step _ Unreachable = return Trap
        step ctx Nop = return $ Done ctx
        step ctx (Block resType expr) = do
            res <- go ctx { labels = Label resType : labels ctx } expr
            case res of
                Break 0 r EvalCtx{ locals = ls } -> return $ Done ctx { locals = ls, stack = r ++ stack ctx }
                Break n r ctx' -> return $ Break (n - 1) r ctx'
                command -> return command
        step ctx loop@(Loop resType expr) = do
            res <- go ctx { labels = Label resType : labels ctx } expr
            case res of
                Break 0 r EvalCtx{ locals = ls } -> step ctx { locals = ls, stack = r ++ stack ctx } loop
                Break n r ctx' -> return $ Break (n - 1) r ctx'
                command -> return command
        step ctx@EvalCtx{ stack = (VI32 v): rest } (If resType true false) = do
            let expr = if v /= 0 then true else false
            res <- go ctx { labels = Label resType : labels ctx, stack = rest } expr
            case res of
                Break 0 r EvalCtx{ locals = ls } -> return $ Done ctx { locals = ls, stack = r ++ stack ctx }
                Break n r ctx' -> return $ Break (n - 1) r ctx'
                command -> return command
        step ctx@EvalCtx{ stack, labels } (Br label) = do
            let idx = fromIntegral label
            let Label resType = labels !! idx
            return $ Break idx (zipWith checkValType resType $ take (length resType) stack) ctx
        step ctx@EvalCtx{ stack = (VI32 v): rest } (BrIf label) =
            if v == 0
            then return $ Done ctx { stack = rest }
            else step ctx { stack = rest } (Br label)
        step ctx@EvalCtx{ stack = (VI32 v): rest } (BrTable labels label) =
            let idx = fromIntegral v in
            let lbl = fromIntegral $ if idx < length labels then labels !! idx else label in
            step ctx { stack = rest } (Br lbl)
        step EvalCtx{ stack } Return =
            let resType = results funcType in
            return $ ReturnFn $ reverse $ zipWith checkValType resType $ take (length resType) stack
        step ctx (Call fun) = do
            let funInst@FunctionInstance { funcType } = funcInstances store ! (funcaddrs moduleInstance ! fromIntegral fun)
            let args = params funcType
            res <- eval store funInst (zipWith checkValType args $ take (length args) $ stack ctx)
            return $ Done ctx { stack = reverse res ++ (drop (length args) $ stack ctx) }
        step ctx@EvalCtx{ stack = (VI32 v): rest } (CallIndirect typeIdx) = do
            let funcType = funcTypes moduleInstance ! fromIntegral typeIdx
            let TableInstance { elements } = tableInstances store ! (tableaddrs moduleInstance ! fromIntegral v)
            let funcAddr = elements !? fromIntegral v
            case funcAddr of
                Just (Just addr) -> do
                    let args = params funcType
                    res <- invoke store addr (zipWith checkValType args $ take (length args) rest)
                    return $ Done ctx { stack = reverse res ++ (drop (length args) rest) }
                _ -> return Trap
        step ctx@EvalCtx{ stack = (_:rest) } Drop = return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 test:val2:val1:rest) } Select =
            if test == 0
            then return $ Done ctx { stack = val1 : rest }
            else return $ Done ctx { stack = val2 : rest }
        step ctx (GetLocal i) = return $ Done ctx { stack = (locals ctx ! fromIntegral i) : stack ctx }
        step ctx@EvalCtx{ stack = (v:rest) } (SetLocal i) =
            return $ Done ctx { stack = rest, locals = locals ctx // [(fromIntegral i, v)] }
        step ctx@EvalCtx{ locals = ls, stack = (v:rest) } (TeeLocal i) =
            return $ Done ctx {
                stack = (ls ! fromIntegral i) : rest,
                locals = locals ctx // [(fromIntegral i, v)]
            }
        step ctx (GetGlobal i) = do
            let globalInst = globalInstances store ! (globaladdrs moduleInstance ! fromIntegral i)
            val <- case globalInst of
                GIConst v -> return v
                GIMut ref -> readIORef ref
            return $ Done ctx { stack = val : stack ctx }
        step ctx@EvalCtx{ stack = (v:rest) } (SetGlobal i) = do
            let globalInst = globalInstances store ! (globaladdrs moduleInstance ! fromIntegral i)
            case globalInst of
                GIConst v -> error "Attempt of mutation of constant global"
                GIMut ref -> writeIORef ref v
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            val <- sum <$> mapM readByte [0..3]
            return $ Done ctx { stack = VI32 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            val <- sum <$> mapM readByte [0..7]
            return $ Done ctx { stack = VI64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (F32Load MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            val <- wordToFloat . sum <$> mapM readByte [0..3]
            return $ Done ctx { stack = VF32 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (F64Load MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            val <- wordToDouble . sum <$> mapM readByte [0..7]
            return $ Done ctx { stack = VF64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load8U MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            byte <- IOVector.read memory addr
            return $ Done ctx { stack = VI32 (fromIntegral byte) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load8S MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            byte <- IOVector.read memory addr
            let val = asWord32 $ if byte >= 128 then -1 * fromIntegral (v .&. 0x7F) else fromIntegral v
            return $ Done ctx { stack = VI32 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load16U MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            val <- sum <$> mapM readByte [0..1]
            return $ Done ctx { stack = VI32 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load16S MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ (fromIntegral byte :: Word32) `shiftL` (idx * 8)
            val <- sum <$> mapM readByte [0..1]
            let signed = asWord32 $ if val >= 2 ^ 15 then -1 * fromIntegral (val .&. 0x7FFF) else fromIntegral val
            return $ Done ctx { stack = VI32 signed : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load8U MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            byte <- IOVector.read memory addr
            return $ Done ctx { stack = VI64 (fromIntegral byte) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load8S MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            byte <- IOVector.read memory addr
            let val = asWord64 $ if byte >= 128 then -1 * fromIntegral (v .&. 0x7F) else fromIntegral v
            return $ Done ctx { stack = VI64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load16U MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            val <- sum <$> mapM readByte [0..1]
            return $ Done ctx { stack = VI64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load16S MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ (fromIntegral byte :: Word32) `shiftL` (idx * 8)
            val <- sum <$> mapM readByte [0..1]
            let signed = asWord64 $ if val >= 2 ^ 15 then -1 * fromIntegral (val .&. 0x7FFF) else fromIntegral val
            return $ Done ctx { stack = VI64 signed : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load32U MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            val <- sum <$> mapM readByte [0..3]
            return $ Done ctx { stack = VI64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load32S MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ (fromIntegral byte :: Word32) `shiftL` (idx * 8)
            val <- sum <$> mapM readByte [0..3]
            let signed = asWord64 $ fromIntegral $ asInt32 val
            return $ Done ctx { stack = VI64 signed : rest }
        step ctx@EvalCtx{ stack = (VI32 v:VI32 va:rest) } (I32Store MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            mapM_ writeByte [0..3]
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI64 v:VI32 va:rest) } (I64Store MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            mapM_ writeByte [0..7]
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VF32 f:VI32 va:rest) } (F32Store MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ va + fromIntegral offset
            let v = floatToWord f
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            mapM_ writeByte [0..3]
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VF64 f:VI32 va:rest) } (F64Store MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ va + fromIntegral offset
            let v = doubleToWord f
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            mapM_ writeByte [0..7]
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 v:VI32 va:rest) } (I32Store8 MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            mapM_ writeByte [0]
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 v:VI32 va:rest) } (I32Store16 MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            mapM_ writeByte [0, 1]
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI64 v:VI32 va:rest) } (I64Store8 MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            mapM_ writeByte [0]
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI64 v:VI32 va:rest) } (I64Store16 MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            mapM_ writeByte [0, 1]
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI64 v:VI32 va:rest) } (I64Store32 MemArg { offset }) = do
            let MemoryInstance { memory } = memInstances store ! (memaddrs moduleInstance ! 0)
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            mapM_ writeByte [0..3]
            return $ Done ctx { stack = rest }
        step ctx (I32Const v) = return $ Done ctx { stack = VI32 v : stack ctx }
        step ctx (I64Const v) = return $ Done ctx { stack = VI64 v : stack ctx }
        step ctx (F32Const v) = return $ Done ctx { stack = VF32 v : stack ctx }
        step ctx (F64Const v) = return $ Done ctx { stack = VF64 v : stack ctx }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IAdd) =
            return $ Done ctx { stack = VI32 (v1 + v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 ISub) =
            return $ Done ctx { stack = VI32 (v1 - v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IMul) =
            return $ Done ctx { stack = VI32 (v1 * v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IDivU) =
            return $ Done ctx { stack = VI32 (v1 `div` v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IDivS) =
            return $ Done ctx { stack = VI32 (asWord32 $ asInt32 v1 `div` asInt32 v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IRemU) =
            return $ Done ctx { stack = VI32 (v1 `rem` v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IRemS) =
            return $ Done ctx { stack = VI32 (asWord32 $ asInt32 v1 `rem` asInt32 v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IAnd) =
            return $ Done ctx { stack = VI32 (v1 .&. v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IOr) =
            return $ Done ctx { stack = VI32 (v1 .|. v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IXor) =
            return $ Done ctx { stack = VI32 (v1 `xor` v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IShl) =
            return $ Done ctx { stack = VI32 (v1 `shiftL` fromIntegral v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IShrU) =
            return $ Done ctx { stack = VI32 (v1 `shiftR` fromIntegral v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IShrS) =
            return $ Done ctx { stack = VI32 (asWord32 $ asInt32 v1 `shiftR` (fromIntegral $ asInt32 v2)) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IRotl) =
            return $ Done ctx { stack = VI32 (v1 `rotateL` fromIntegral v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IRotr) =
            return $ Done ctx { stack = VI32 (v1 `rotateR` fromIntegral v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 IEq) =
            return $ Done ctx { stack = VI32 (if v1 == v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 INe) =
            return $ Done ctx { stack = VI32 (if v1 /= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 ILtU) =
            return $ Done ctx { stack = VI32 (if v1 < v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 ILtS) =
            return $ Done ctx { stack = VI32 (if asInt32 v1 < asInt32 v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 IGtU) =
            return $ Done ctx { stack = VI32 (if v1 > v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 IGtS) =
            return $ Done ctx { stack = VI32 (if asInt32 v1 > asInt32 v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 ILeU) =
            return $ Done ctx { stack = VI32 (if v1 <= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 ILeS) =
            return $ Done ctx { stack = VI32 (if asInt32 v1 <= asInt32 v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 IGeU) =
            return $ Done ctx { stack = VI32 (if v1 >= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IRelOp BS32 IGeS) =
            return $ Done ctx { stack = VI32 (if asInt32 v1 >= asInt32 v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } I32Eqz =
            return $ Done ctx { stack = VI32 (if v == 0 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (IUnOp BS32 IClz) =
            return $ Done ctx { stack = VI32 (fromIntegral $ countLeadingZeros v) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (IUnOp BS32 ICtz) =
            return $ Done ctx { stack = VI32 (fromIntegral $ countTrailingZeros v) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (IUnOp BS32 IPopcnt) =
            return $ Done ctx { stack = VI32 (fromIntegral $ popCount v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IAdd) =
            return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 + asInt64 v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 ISub) =
            return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 - asInt64 v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IMul) =
            return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 * asInt64 v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IDivU) =
            return $ Done ctx { stack = VI64 (v1 `div` v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IDivS) =
            return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 `div` asInt64 v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IRemU) =
            return $ Done ctx { stack = VI64 (v1 `rem` v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IRemS) =
            return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 `rem` asInt64 v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IAnd) =
            return $ Done ctx { stack = VI64 (v1 .&. v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IOr) =
            return $ Done ctx { stack = VI64 (v1 .|. v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IXor) =
            return $ Done ctx { stack = VI64 (v1 `xor` v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IShl) =
            return $ Done ctx { stack = VI64 (v1 `shiftL` fromIntegral v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IShrU) =
            return $ Done ctx { stack = VI64 (v1 `shiftR` fromIntegral v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IShrS) =
            return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 `shiftR` (fromIntegral $ asInt64 v2)) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IRotl) =
            return $ Done ctx { stack = VI64 (v1 `rotateL` fromIntegral v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IRotr) =
            return $ Done ctx { stack = VI64 (v1 `rotateR` fromIntegral v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 IEq) =
            return $ Done ctx { stack = VI32 (if v1 == v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 INe) =
            return $ Done ctx { stack = VI32 (if v1 /= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 ILtU) =
            return $ Done ctx { stack = VI32 (if v1 < v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 ILtS) =
            return $ Done ctx { stack = VI32 (if asInt64 v1 < asInt64 v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 IGtU) =
            return $ Done ctx { stack = VI32 (if v1 > v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 IGtS) =
            return $ Done ctx { stack = VI32 (if asInt64 v1 > asInt64 v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 ILeU) =
            return $ Done ctx { stack = VI32 (if v1 <= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 ILeS) =
            return $ Done ctx { stack = VI32 (if asInt64 v1 <= asInt64 v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 IGeU) =
            return $ Done ctx { stack = VI32 (if v1 >= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IRelOp BS64 IGeS) =
            return $ Done ctx { stack = VI32 (if asInt64 v1 >= asInt64 v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } I64Eqz =
            return $ Done ctx { stack = VI32 (if v == 0 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (IUnOp BS64 IClz) =
            return $ Done ctx { stack = VI32 (fromIntegral $ countLeadingZeros v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (IUnOp BS64 ICtz) =
            return $ Done ctx { stack = VI32 (fromIntegral $ countTrailingZeros v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (IUnOp BS64 IPopcnt) =
            return $ Done ctx { stack = VI32 (fromIntegral $ popCount v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FAbs) =
            return $ Done ctx { stack = VF32 (abs v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FNeg) =
            return $ Done ctx { stack = VF32 (negate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FCeil) =
            return $ Done ctx { stack = VF32 (fromIntegral (ceiling v :: Int)) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FFloor) =
            return $ Done ctx { stack = VF32 (fromIntegral (floor v :: Int)) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FTrunc) =
            return $ Done ctx { stack = VF32 (fromIntegral (truncate v :: Int)) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FNearest) =
            return $ Done ctx { stack = VF32 (nearest v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FSqrt) =
            return $ Done ctx { stack = VF32 (sqrt v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FAbs) =
            return $ Done ctx { stack = VF64 (abs v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FNeg) =
            return $ Done ctx { stack = VF64 (negate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FCeil) =
            return $ Done ctx { stack = VF64 (fromIntegral (ceiling v :: Int64)) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FFloor) =
            return $ Done ctx { stack = VF64 (fromIntegral (floor v :: Int64)) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FTrunc) =
            return $ Done ctx { stack = VF64 (fromIntegral (truncate v :: Int64)) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FNearest) =
            return $ Done ctx { stack = VF64 (nearest v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FSqrt) =
            return $ Done ctx { stack = VF64 (sqrt v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FBinOp BS32 FAdd) =
            return $ Done ctx { stack = VF32 (v1 + v2) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FBinOp BS32 FSub) =
            return $ Done ctx { stack = VF32 (v1 - v2) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FBinOp BS32 FMul) =
            return $ Done ctx { stack = VF32 (v1 * v2) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FBinOp BS32 FDiv) =
            return $ Done ctx { stack = VF32 (v1 / v2) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FBinOp BS32 FMin) =
            return $ Done ctx { stack = VF32 (min v1 v2) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FBinOp BS32 FMax) =
            return $ Done ctx { stack = VF32 (max v1 v2) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FBinOp BS32 FCopySign) =
            return $ Done ctx { stack = VF32 (abs v1 * signum v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FAdd) =
            return $ Done ctx { stack = VF64 (v1 + v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FSub) =
            return $ Done ctx { stack = VF64 (v1 - v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FMul) =
            return $ Done ctx { stack = VF64 (v1 * v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FDiv) =
            return $ Done ctx { stack = VF64 (v1 / v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FMin) =
            return $ Done ctx { stack = VF64 (min v1 v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FMax) =
            return $ Done ctx { stack = VF64 (max v1 v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FCopySign) =
            return $ Done ctx { stack = VF64 (abs v1 * signum v2) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FRelOp BS32 FEq) =
            return $ Done ctx { stack = VI32 (if v1 == v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FRelOp BS32 FNe) =
            return $ Done ctx { stack = VI32 (if v1 /= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FRelOp BS32 FLt) =
            return $ Done ctx { stack = VI32 (if v1 < v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FRelOp BS32 FGt) =
            return $ Done ctx { stack = VI32 (if v1 > v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FRelOp BS32 FLe) =
            return $ Done ctx { stack = VI32 (if v1 <= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FRelOp BS32 FGe) =
            return $ Done ctx { stack = VI32 (if v1 >= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FRelOp BS64 FEq) =
            return $ Done ctx { stack = VI32 (if v1 == v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FRelOp BS64 FNe) =
            return $ Done ctx { stack = VI32 (if v1 /= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FRelOp BS64 FLt) =
            return $ Done ctx { stack = VI32 (if v1 < v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FRelOp BS64 FGt) =
            return $ Done ctx { stack = VI32 (if v1 > v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FRelOp BS64 FLe) =
            return $ Done ctx { stack = VI32 (if v1 <= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FRelOp BS64 FGe) =
            return $ Done ctx { stack = VI32 (if v1 >= v2 then 1 else 0) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } I32WrapI64 =
            return $ Done ctx { stack = VI32 (fromIntegral $ v .&. 0xFFFFFFFF) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFU BS32 BS32) =
            return $ Done ctx { stack = VI32 (round v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFU BS32 BS64) =
            return $ Done ctx { stack = VI32 (round v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFU BS64 BS32) =
            return $ Done ctx { stack = VI64 (round v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFU BS64 BS64) =
            return $ Done ctx { stack = VI64 (round v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFS BS32 BS32) =
            return $ Done ctx { stack = VI32 (asWord32 $ round v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFS BS32 BS64) =
            return $ Done ctx { stack = VI32 (asWord32 $ round v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFS BS64 BS32) =
            return $ Done ctx { stack = VI64 (asWord64 $ round v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFS BS64 BS64) =
            return $ Done ctx { stack = VI64 (asWord64 $ round v) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } I64ExtendUI32 =
            return $ Done ctx { stack = VI64 (fromIntegral v) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } I64ExtendSI32 =
            return $ Done ctx { stack = VI64 (asWord64 $ fromIntegral $ asInt32 v) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (FConvertIU BS32 BS32) =
            return $ Done ctx { stack = VF32 (realToFrac v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (FConvertIU BS32 BS64) =
            return $ Done ctx { stack = VF32 (realToFrac v) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (FConvertIU BS64 BS32) =
            return $ Done ctx { stack = VF64 (realToFrac v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (FConvertIU BS64 BS64) =
            return $ Done ctx { stack = VF64 (realToFrac v) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (FConvertIS BS32 BS32) =
            return $ Done ctx { stack = VF32 (realToFrac $ asInt32 v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (FConvertIS BS32 BS64) =
            return $ Done ctx { stack = VF32 (realToFrac $ asInt64 v) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (FConvertIS BS64 BS32) =
            return $ Done ctx { stack = VF64 (realToFrac $ asInt32 v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (FConvertIS BS64 BS64) =
            return $ Done ctx { stack = VF64 (realToFrac $ asInt64 v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } F32DemoteF64 =
            return $ Done ctx { stack = VF32 (realToFrac v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } F64PromoteF32 =
            return $ Done ctx { stack = VF64 (realToFrac v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (IReinterpretF BS32) =
            return $ Done ctx { stack = VI32 (floatToWord v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (IReinterpretF BS64) =
            return $ Done ctx { stack = VI64 (doubleToWord v) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (FReinterpretI BS32) =
            return $ Done ctx { stack = VF32 (wordToFloat v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (FReinterpretI BS64) =
            return $ Done ctx { stack = VF64 (wordToDouble v) : rest }
        step _   instr = error $ "Error during evaluation of instruction: " ++ show instr
eval store HostInstance { funcType, tag } args = return args

invoke :: Store -> Address -> [Value] -> IO [Value]
invoke st funcIdx = eval st $ funcInstances st ! funcIdx

invokeExport :: Store -> ModuleInstance -> TL.Text -> [Value] -> IO [Value]
invokeExport st ModuleInstance { exports } name args =
    case Vector.find (\(ExportInstance n _) -> n == name) exports of
        Just (ExportInstance _ (ExternFunction addr)) -> invoke st addr args
        _ -> error $ "Function with name " ++ show name ++ " was not found in module's exports"