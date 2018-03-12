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
import Data.IORef (IORef, newIORef, readIORef)
import Data.Word (Word8, Word32, Word64)
import Numeric.Natural (Natural)
import qualified Control.Monad as Monad
import Data.Monoid ((<>))
import qualified Data.List as List
import Data.Bits ((.|.), shiftL)

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
    memory :: IOVector Word32,
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

pageSizeInWord32 :: Int
pageSizeInWord32 = 16 * 1024

allocMems :: [Memory] -> IO (Vector MemoryInstance)
allocMems mems = Vector.fromList <$> mapM allocMem mems
    where
        allocMem :: Memory -> IO MemoryInstance
        allocMem (Memory (Limit from to)) = do
            memory <- IOVector.replicate (fromIntegral from * pageSizeInWord32) 0
            return $ MemoryInstance {
                memory,
                maxLen = fromIntegral <$> to
            }

initialize :: ModuleInstance -> Module -> Store -> IO Store
initialize inst Module {elems, datas} store = do
    store' <- Monad.foldM initElem store elems
    Monad.foldM initData store' datas
    where
        fitOrGrowTable :: Address -> Store -> Int -> TableInstance
        fitOrGrowTable idx st last =
            let t@(TableInstance elems maxLen) = tableInstances st ! idx in
            let len = Vector.length elems in
            let increased = TableInstance (elems Vector.++ (Vector.fromList $ replicate (last - len + 1) Nothing)) maxLen in
            if last < len
                then t
                else case maxLen of
                    Nothing -> increased
                    Just max -> if max <= last then error "Max table length reached" else increased

        initElem :: Store -> ElemSegment -> IO Store
        initElem st ElemSegment {tableIndex, offset, funcIndexes} = do
            VI32 val <- evalConstExpr inst store offset
            let from = fromIntegral val
            let funcs = map ((funcaddrs inst !) . fromIntegral) funcIndexes
            let idx = tableaddrs inst ! fromIntegral tableIndex
            let TableInstance elems maxLen = fitOrGrowTable idx st (from + length funcs)
            let table = TableInstance (elems Vector.// zip [from..] (map Just funcs)) maxLen
            return $ st { tableInstances = tableInstances st Vector.// [(idx, table)] }
        
        fitOrGrowMemory :: Address -> Store -> Int -> IO MemoryInstance
        fitOrGrowMemory idx st last = do
            let m@(MemoryInstance mem maxLen) = memInstances st ! idx
            let len = IOVector.length mem
            let increased = do
                    let pages = (last - len) `div` pageSizeInWord32 + (if (last - len) `rem` len == 0 then 0 else 1)
                    mem' <- IOVector.grow mem $ pages * pageSizeInWord32
                    return $ MemoryInstance mem' maxLen
            if last < len
                then return m
                else case maxLen of
                    Nothing -> increased
                    Just max -> if max * pageSizeInWord32 <= last then error "Max memory length reached" else increased

        initData :: Store -> DataSegment -> IO Store
        initData st DataSegment {memIndex, offset, chunk} = do
            VI32 val <- evalConstExpr inst store offset
            let from = fromIntegral val
            let idx = memaddrs inst ! fromIntegral memIndex
            let last = from + (fromIntegral $ LBS.length chunk) `div` 4 + if LBS.length chunk `rem` 4 == 0 then 0 else 1
            MemoryInstance mem maxLen <- fitOrGrowMemory idx st last
            setBytes mem from $ LBS.unpack chunk
            return $ st { memInstances = memInstances st // [(idx, MemoryInstance mem maxLen)] }
        
        setBytes :: IOVector Word32 -> Int -> [Word8] -> IO ()
        setBytes _ _ [] = return ()
        setBytes v off bytes = do
            let parts = zip [3, 2, 1, 0] $ take 4 bytes ++ [0, 0, 0] -- to fill if len < 4, zip cuts unneeded
            let word = List.foldl' (\w (sh, b) -> w .|. (fromIntegral b `shiftL` (8 * sh)) ) 0 parts
            IOVector.write v off word
            setBytes v (off + 1) $ drop 4 bytes

data EvalContext = EvalContext ModuleInstance (IORef Store)

instantiate :: Store -> Imports -> Module -> IO EvalContext
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
    ref <- newIORef st'
    return $ EvalContext inst ref

invoke :: EvalContext -> TL.Text -> [Value] -> IO [Value]
invoke = undefined