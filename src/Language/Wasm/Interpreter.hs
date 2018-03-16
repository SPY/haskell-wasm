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
            if v /= 0
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
        step ctx (I32Const v) = return $ Done ctx { stack = VI32 v : stack ctx }
        step ctx (I64Const v) = return $ Done ctx { stack = VI64 v : stack ctx }
        step ctx (F32Const v) = return $ Done ctx { stack = VF32 v : stack ctx }
        step ctx (F64Const v) = return $ Done ctx { stack = VF64 v : stack ctx }
        step _   instr = error $ "Error during evaluation of instruction " ++ show instr
eval store HostInstance { funcType, tag } args = return args

invoke :: Store -> Address -> [Value] -> IO [Value]
invoke st funcIdx = eval st $ funcInstances st ! funcIdx

invokeExport :: Store -> TL.Text -> [Value] -> IO [Value]
invokeExport = undefined