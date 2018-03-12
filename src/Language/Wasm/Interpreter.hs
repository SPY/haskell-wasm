{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Wasm.Interpreter (
    instantiate
) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as LBS

import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as Vector
import Data.IORef (IORef, newIORef, readIORef)
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import Data.Word (Word32, Word64)
import Numeric.Natural (Natural)

import Language.Wasm.Structure as Struct

data Value =
    VI32 Word32
    | VI64 Word64
    | VF32 Float
    | VF64 Double
    deriving (Eq, Show)

data AdminInstr =
    I Instruction
    | Trap
    | Invoke Address
    | InitElem Address Word32 [Natural]
    | InitData Address Word32 LBS.ByteString
    | Label [AdminInstr] [AdminInstr]
    | IFrame Frame [AdminInstr]
    deriving (Show, Eq)

data Frame = Frame { locals :: Vector Value, mod :: ModuleInstance } deriving (Eq, Show)

type Address = Int

data TableInstance = TableInstance {
    elements :: Vector (Maybe Address),
    maxLen :: Maybe Int
}

data MemoryInstance = MemoryInstance {
    memory :: IOArray Int Word32,
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

allocGlobals :: ModuleInstance -> Store -> [Global] -> IO (Vector GlobalInstance)
allocGlobals inst store globs = Vector.fromList <$> mapM allocGlob globs
    where
        runIniter :: [Instruction] -> IO Value
        -- due the validation there can be only these instructions
        runIniter [I32Const v] = return $ VI32 v
        runIniter [I64Const v] = return $ VI64 v
        runIniter [F32Const v] = return $ VF32 v
        runIniter [F64Const v] = return $ VF64 v
        -- the spec says get global can ref only imported globals
        runIniter [GetGlobal i] = getGlobalValue inst store i
        runIniter instrs = error $ "Global initializer contains unsupported instructions: " ++ show instrs

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

allocMems :: [Memory] -> IO (Vector MemoryInstance)
allocMems mems = Vector.fromList <$> mapM allocMem mems
    where
        allocMem :: Memory -> IO MemoryInstance
        allocMem (Memory (Limit from to)) = do
            memory <- newArray (0, fromIntegral from * (16 * 1024{- 64 * 1024 bytes in Word32 -})) 0
            return $ MemoryInstance {
                memory,
                maxLen = fromIntegral <$> to
            }

instantiate :: Store -> Imports -> Module -> IO (ModuleInstance, Store)
instantiate st imps m = do
    let inst = calcInstance st imps m
    let functions = funcInstances st Vector.++ (allocFunctions inst $ Struct.functions m)
    globals <- (globalInstances st Vector.++) <$> (allocGlobals inst st $ Struct.globals m)
    let tables = tableInstances st Vector.++ (allocTables $ Struct.tables m)
    mems <- (memInstances st Vector.++) <$> (allocMems $ Struct.mems m)
    let st' = st {
        funcInstances = functions,
        tableInstances = tables,
        memInstances = mems,
        globalInstances = globals
    }
    return $ (inst, st')
