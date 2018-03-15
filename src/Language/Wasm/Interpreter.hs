{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Wasm.Interpreter (
    instantiate,
    invoke
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
import Numeric.Natural (Natural)
import qualified Control.Monad as Monad
import Data.Monoid ((<>))

import Language.Wasm.Structure as Struct

data Value =
    VI32 Word32
    | VI64 Word64
    | VF32 Float
    | VF64 Double
    deriving (Eq, Show)

data Label = Label

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
    types :: Vector FuncType,
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
        types = Vector.fromList types,
        funcaddrs = funs,
        tableaddrs = tbls,
        memaddrs = memories,
        globaladdrs = globs,
        exports = Vector.fromList $ map refExport exports
    }

type Imports = Map.Map (TL.Text, TL.Text) ExternalValue

allocFunctions :: ModuleInstance -> [Function] -> Vector FunctionInstance
allocFunctions inst@ModuleInstance {types} funs =
    let mkFuncInst f@Function {funcType} = FunctionInstance (types ! (fromIntegral funcType)) inst f in
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
        initElem :: Store -> ElemSegment -> IO Store
        initElem st ElemSegment {tableIndex, offset, funcIndexes} = do
            VI32 val <- evalConstExpr inst store offset
            let from = fromIntegral val
            let funcs = map ((funcaddrs inst !) . fromIntegral) funcIndexes
            let idx = tableaddrs inst ! fromIntegral tableIndex
            let TableInstance elems maxLen = tableInstances st ! idx
            let len = Vector.length elems
            if from + length funcs >= len
            then error "Element indexes are out of the table bounds"
            else do
                let table = TableInstance (elems // zip [from..] (map Just funcs)) maxLen
                return $ st { tableInstances = tableInstances st Vector.// [(idx, table)] }

        initData :: Store -> DataSegment -> IO Store
        initData st DataSegment {memIndex, offset, chunk} = do
            VI32 val <- evalConstExpr inst store offset
            let from = fromIntegral val
            let idx = memaddrs inst ! fromIntegral memIndex
            let last = from + (fromIntegral $ LBS.length chunk)
            let MemoryInstance mem maxLen = memInstances st ! idx
            let len = IOVector.length mem
            if last >= len
            then error "Data chunk is out of the memory bounds"
            else do
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
}

eval :: Store -> FunctionInstance -> [Value] -> IO [Value]
eval store FunctionInstance { funcType, moduleInstance, code = Function { localTypes, body} } args = do
    let checkedArgs = zipWith checkArgType (params funcType) args
    let initialContext = EvalCtx {
            locals = Vector.fromList $ checkedArgs ++ map initLocal localTypes,
            labels = [],
            stack = []
        }
    result <- Monad.foldM step initialContext body
    return $ reverse $ stack result
    where
        checkArgType :: ValueType -> Value -> Value
        checkArgType I32 (VI32 v) = VI32 v
        checkArgType I64 (VI64 v) = VI64 v
        checkArgType F32 (VF32 v) = VF32 v
        checkArgType F64 (VF64 v) = VF64 v
        checkArgType _   _        = error "Argument types do not match function type"

        initLocal :: ValueType -> Value
        initLocal I32 = VI32 0
        initLocal I64 = VI64 0
        initLocal F32 = VF32 0
        initLocal F64 = VF64 0

        step :: EvalCtx -> Instruction -> IO EvalCtx
        step ctx (I32Const v) = return ctx { stack = VI32 v : stack ctx }
        step ctx (I64Const v) = return ctx { stack = VI64 v : stack ctx }
        step ctx (F32Const v) = return ctx { stack = VF32 v : stack ctx }
        step ctx (F64Const v) = return ctx { stack = VF64 v : stack ctx }
        step ctx (GetLocal i) = return ctx { stack = (locals ctx ! fromIntegral i) : stack ctx }
        step ctx@EvalCtx{ stack = (v:rest) } (SetLocal i) =
            return ctx { stack = rest, locals = locals ctx // [(fromIntegral i, v)] }
        step ctx@EvalCtx{ locals = ls, stack = (v:rest) } (TeeLocal i) =
            return ctx {
                stack = (ls ! fromIntegral i) : rest,
                locals = locals ctx // [(fromIntegral i, v)]
            }
        step ctx (GetGlobal i) = do
            let globalInst = globalInstances store ! (globaladdrs moduleInstance ! fromIntegral i)
            val <- case globalInst of
                GIConst v -> return v
                GIMut ref -> readIORef ref
            return ctx { stack = val : stack ctx }
        step ctx@EvalCtx{ stack = (v:rest) } (SetGlobal i) = do
            let globalInst = globalInstances store ! (globaladdrs moduleInstance ! fromIntegral i)
            case globalInst of
                GIConst v -> error "Attempt of mutation of constant global"
                GIMut ref -> writeIORef ref v
            return ctx { stack = rest }
        step _   _ = error "Error during evaluation"
eval store HostInstance { funcType, tag } args = return args

invoke :: Store -> Address -> [Value] -> IO [Value]
invoke st funcIdx = eval st $ funcInstances st ! funcIdx

invokeExport :: Store -> TL.Text -> [Value] -> IO [Value]
invokeExport = undefined