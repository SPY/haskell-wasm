{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Wasm.Interpreter (
    Value(..),
    Store,
    ModuleInstance(..),
    ExternalValue(..),
    ExportInstance(..),
    GlobalInstance(..),
    Imports,
    HostItem(..),
    instantiate,
    invoke,
    invokeExport,
    getGlobalValueByName,
    emptyStore,
    emptyImports,
    makeHostModule,
    makeMutGlobal
) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing)

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
import Numeric.IEEE (IEEE, copySign, minNum, maxNum, identicalIEEE)

import Debug.Trace as Debug

import Language.Wasm.Structure as Struct
import Language.Wasm.FloatUtils (
        wordToFloat,
        floatToWord,
        wordToDouble,
        doubleToWord
    )

data Value =
    VI32 Word32
    | VI64 Word64
    | VF32 Float
    | VF64 Double
    deriving (Eq, Show)

asInt32 :: Word32 -> Int32
asInt32 w =
    if w < 0x80000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFF - w + 1)

asInt64 :: Word64 -> Int64
asInt64 w =
    if w < 0x8000000000000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFFFFFFFFFF - w + 1)

asWord32 :: Int32 -> Word32
asWord32 i
    | i >= 0 = fromIntegral i
    | otherwise = 0xFFFFFFFF - (fromIntegral (abs i)) + 1

asWord64 :: Int64 -> Word64
asWord64 i
    | i >= 0 = fromIntegral i
    | otherwise = 0xFFFFFFFFFFFFFFFF - (fromIntegral (abs i)) + 1

nearest :: (IEEE a) => a -> a
nearest f
    | isNaN f = f
    | f >= 0 && f <= 0.5 = copySign 0 f
    | f < 0 && f >= -0.5 = -0
    | otherwise =
        let i = floor f :: Integer in
        let fi = fromIntegral i in
        let r = abs f - abs fi in
        flip copySign f $ (
            if r == 0.5
            then (
                case (even i, f < 0) of
                    (True, _) -> fi
                    (_, True) -> fi - 1.0
                    (_, False) -> fi + 1.0
            )
            else fromIntegral (round f :: Integer)
        )

zeroAwareMin :: IEEE a => a -> a -> a
zeroAwareMin a b
    | identicalIEEE a 0 && identicalIEEE b (-0) = b
    | isNaN a = a
    | isNaN b = b
    | otherwise = minNum a b

zeroAwareMax :: IEEE a => a -> a -> a
zeroAwareMax a b
    | identicalIEEE a (-0) && identicalIEEE b 0 = b
    | isNaN a = a
    | isNaN b = b
    | otherwise = maxNum a b

floatFloor :: Float -> Float
floatFloor a
    | isNaN a = a
    | otherwise = copySign (fromIntegral (floor a :: Integer)) a

doubleFloor :: Double -> Double
doubleFloor a
    | isNaN a = a
    | otherwise = copySign (fromIntegral (floor a :: Integer)) a

floatCeil :: Float -> Float
floatCeil a
    | isNaN a = a
    | otherwise = copySign (fromIntegral (ceiling a :: Integer)) a

doubleCeil :: Double -> Double
doubleCeil a
    | isNaN a = a
    | otherwise = copySign (fromIntegral (ceiling a :: Integer)) a

floatTrunc :: Float -> Float
floatTrunc a
    | isNaN a = a
    | otherwise = copySign (fromIntegral (truncate a :: Integer)) a

doubleTrunc :: Double -> Double
doubleTrunc a
    | isNaN a = a
    | otherwise = copySign (fromIntegral (truncate a :: Integer)) a

data Label = Label ResultType deriving (Show, Eq)

type Address = Int

data TableInstance = TableInstance {
    lim :: Limit,
    elements :: Vector (Maybe Address)
}

data MemoryInstance = MemoryInstance {
    lim :: Limit,
    memory :: IORef (IOVector Word8)
}

data GlobalInstance = GIConst ValueType Value | GIMut ValueType (IORef Value)

makeMutGlobal :: Value -> IO GlobalInstance
makeMutGlobal val = GIMut (getValueType val) <$> newIORef val

getValueType :: Value -> ValueType
getValueType (VI32 _) = I32
getValueType (VI64 _) = I64
getValueType (VF32 _) = F32
getValueType (VF64 _) = F64

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
        hostCode :: HostFunction
    }

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

type HostFunction = [Value] -> IO [Value]

data HostItem
    = HostFunction FuncType HostFunction
    | HostGlobal GlobalInstance
    | HostMemory Limit
    | HostTable Limit

makeHostModule :: Store -> [(TL.Text, HostItem)] -> IO (Store, ModuleInstance)
makeHostModule st items = do
    let (st', inst') = makeHostFunctions st emptyModInstance
    let (st'', inst'') = makeHostGlobals st' inst'
    (st''', inst''') <- makeHostMems st'' inst''
    makeHostTables st''' inst'''
    where
        hostFunctions :: [(TL.Text, HostItem)]
        hostFunctions = filter isHostFunction items

        isHostFunction :: (TL.Text, HostItem) -> Bool
        isHostFunction (_, (HostFunction _ _)) = True
        isHostFunction _ = False

        makeHostFunctions :: Store -> ModuleInstance -> (Store, ModuleInstance)
        makeHostFunctions st inst =
            let funcLen = Vector.length $ funcInstances st in
            let instances = map (\(_, (HostFunction t c)) -> HostInstance t c) hostFunctions in
            let types = map (\(_, (HostFunction t _)) -> t) hostFunctions in
            let exps = Vector.fromList $ zipWith (\(name, _) i -> ExportInstance name (ExternFunction i)) hostFunctions [funcLen..] in
            let inst' = inst {
                    funcTypes = Vector.fromList types,
                    funcaddrs = Vector.fromList [funcLen..funcLen + length instances - 1],
                    exports = Language.Wasm.Interpreter.exports inst <> exps
                }
            in
            let st' = st { funcInstances = funcInstances st <> Vector.fromList instances } in
            (st', inst')
        
        hostGlobals :: [(TL.Text, HostItem)]
        hostGlobals = filter isHostGlobal items

        isHostGlobal :: (TL.Text, HostItem) -> Bool
        isHostGlobal (_, (HostGlobal _)) = True
        isHostGlobal _ = False
        
        makeHostGlobals :: Store -> ModuleInstance -> (Store, ModuleInstance)
        makeHostGlobals st inst =
            let globLen = Vector.length $ globalInstances st in
            let instances = map (\(_, (HostGlobal g)) -> g) hostGlobals in
            let exps = Vector.fromList $ zipWith (\(name, _) i -> ExportInstance name (ExternGlobal i)) hostGlobals [globLen..] in
            let inst' = inst {
                    globaladdrs = Vector.fromList [globLen..globLen + length instances - 1],
                    exports = Language.Wasm.Interpreter.exports inst <> exps
                }
            in
            let st' = st { globalInstances = globalInstances st <> Vector.fromList instances } in
            (st', inst')

        hostMems :: [(TL.Text, HostItem)]
        hostMems = filter isHostMem items

        isHostMem :: (TL.Text, HostItem) -> Bool
        isHostMem (_, (HostMemory _)) = True
        isHostMem _ = False
            
        makeHostMems :: Store -> ModuleInstance -> IO (Store, ModuleInstance)
        makeHostMems st inst = do
            let memLen = Vector.length $ memInstances st
            instances <- allocMems $ map (\(_, (HostMemory lim)) -> Memory lim) hostMems
            let exps = Vector.fromList $ zipWith (\(name, _) i -> ExportInstance name (ExternMemory i)) hostMems [memLen..]
            let inst' = inst {
                    memaddrs = Vector.fromList [memLen..memLen + length instances - 1],
                    exports = Language.Wasm.Interpreter.exports inst <> exps
                }
            let st' = st { memInstances = memInstances st <> instances }
            return (st', inst')
        
        hostTables :: [(TL.Text, HostItem)]
        hostTables = filter isHostTable items

        isHostTable :: (TL.Text, HostItem) -> Bool
        isHostTable (_, (HostTable _)) = True
        isHostTable _ = False
            
        makeHostTables :: Store -> ModuleInstance -> IO (Store, ModuleInstance)
        makeHostTables st inst = do
            let tableLen = Vector.length $ tableInstances st
            let instances = allocTables $ map (\(_, (HostTable lim)) -> Table (TableType lim AnyFunc)) hostTables
            let exps = Vector.fromList $ zipWith (\(name, _) i -> ExportInstance name (ExternTable i)) hostTables [tableLen..]
            let inst' = inst {
                    tableaddrs = Vector.fromList [tableLen..tableLen + length instances - 1],
                    exports = Language.Wasm.Interpreter.exports inst <> exps
                }
            let st' = st { tableInstances = tableInstances st <> instances }
            return (st', inst')

data ModuleInstance = ModuleInstance {
    funcTypes :: Vector FuncType,
    funcaddrs :: Vector Address,
    tableaddrs :: Vector Address,
    memaddrs :: Vector Address,
    globaladdrs :: Vector Address,
    exports :: Vector ExportInstance
} deriving (Eq, Show)

emptyModInstance :: ModuleInstance
emptyModInstance = ModuleInstance {
    funcTypes = Vector.empty,
    funcaddrs = Vector.empty,
    tableaddrs = Vector.empty,
    memaddrs = Vector.empty,
    globaladdrs = Vector.empty,
    exports = Vector.empty
}

calcInstance :: Store -> Imports -> Module -> Either String ModuleInstance
calcInstance (Store fs ts ms gs) imps Module {functions, types, tables, mems, globals, exports, imports} = do
    let funLen = length fs
    let tableLen = length ts
    let memLen = length ms
    let globalLen = length gs
    funImps <- mapM checkImportType $ filter isFuncImport imports
    tableImps <- mapM checkImportType $ filter isTableImport imports
    memImps <- mapM checkImportType $ filter isMemImport imports
    globalImps <- mapM checkImportType $ filter isGlobalImport imports
    let funs = Vector.fromList $ map (\(ExternFunction i) -> i) funImps ++ [funLen..funLen + length functions - 1]
    let tbls = Vector.fromList $ map (\(ExternTable i) -> i) tableImps ++ [tableLen..tableLen + length tables - 1]
    let memories = Vector.fromList $ map (\(ExternMemory i) -> i) memImps ++ [memLen..memLen + length mems - 1]
    let globs = Vector.fromList $ map (\(ExternGlobal i) -> i) globalImps ++ [globalLen..globalLen + length globals - 1]
    let
        refExport (Export name (ExportFunc idx)) =
            ExportInstance name $ ExternFunction $ funs ! fromIntegral idx
        refExport (Export name (ExportTable idx)) =
            ExportInstance name $ ExternTable $ tbls ! fromIntegral idx
        refExport (Export name (ExportMemory idx)) =
            ExportInstance name $ ExternMemory $ memories ! fromIntegral idx
        refExport (Export name (ExportGlobal idx)) =
            ExportInstance name $ ExternGlobal $ globs ! fromIntegral idx
    return $ ModuleInstance {
        funcTypes = Vector.fromList types,
        funcaddrs = funs,
        tableaddrs = tbls,
        memaddrs = memories,
        globaladdrs = globs,
        exports = Vector.fromList $ map refExport exports
    }
    where
        getImpIdx :: Import -> Either String ExternalValue
        getImpIdx (Import m n _) =
            case Map.lookup (m, n) imps of
                Just idx -> Right idx
                Nothing -> Left $ "Cannot find import from module " ++ show m ++ " with name " ++ show n

        checkImportType :: Import -> Either String ExternalValue
        checkImportType imp@(Import _ _ (ImportFunc typeIdx)) = do
            idx <- getImpIdx imp
            funcAddr <- case idx of
                ExternFunction funcAddr -> Right funcAddr
                other -> Left "incompatible import type"
            let expectedType = types !! fromIntegral typeIdx
            let actualType = Language.Wasm.Interpreter.funcType $ fs ! funcAddr
            if expectedType == actualType
            then Right idx
            else Left "incompatible import type"
        checkImportType imp@(Import _ _ (ImportGlobal globalType)) = do
            let err = Left "incompatible import type"
            idx <- getImpIdx imp
            globalAddr <- case idx of
                ExternGlobal globalAddr -> Right globalAddr
                _ -> err
            let globalInst = gs ! globalAddr
            let vt = case globalType of
                    Const vt -> vt
                    Mut vt -> vt
            let vt' = case globalInst of
                    GIConst vt _ -> vt
                    GIMut vt _ -> vt
            if vt == vt' then Right idx else err
        checkImportType imp@(Import _ _ (ImportMemory limit)) = do
            idx <- getImpIdx imp
            memAddr <- case idx of
                ExternMemory memAddr -> Right memAddr
                _ -> Left "incompatible import type"
            let MemoryInstance { lim } = ms ! memAddr
            if limitMatch lim limit
            then Right idx
            else Left "incompatible import type"
        checkImportType imp@(Import _ _ (ImportTable (TableType limit _))) = do
            idx <- getImpIdx imp
            tableAddr <- case idx of
                ExternTable tableAddr -> Right tableAddr
                _ -> Left "incompatible import type"
            let TableInstance { lim } = ts ! tableAddr
            if limitMatch lim limit
            then Right idx
            else Left "incompatible import type"
    
        limitMatch :: Limit -> Limit -> Bool
        limitMatch (Limit n1 m1) (Limit n2 m2) = n1 >= n2 && (isNothing m2 || fromMaybe False ((<=) <$> m1 <*> m2))

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
        GIConst _ v -> return v
        GIMut _ ref -> readIORef ref

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
        allocGlob (Global (Const vt) initer) = GIConst vt <$> runIniter initer
        allocGlob (Global (Mut vt) initer) = do
            val <- runIniter initer
            GIMut vt <$> newIORef val

allocTables :: [Table] -> Vector TableInstance
allocTables tables = Vector.fromList $ map allocTable tables
    where
        allocTable :: Table -> TableInstance
        allocTable (Table (TableType lim@(Limit from to) _)) =
            TableInstance {
                lim,
                elements = Vector.fromList $ replicate (fromIntegral from) Nothing
            }

pageSize :: Int
pageSize = 64 * 1024

allocMems :: [Memory] -> IO (Vector MemoryInstance)
allocMems mems = Vector.fromList <$> mapM allocMem mems
    where
        allocMem :: Memory -> IO MemoryInstance
        allocMem (Memory lim@(Limit from to)) = do
            mem <- IOVector.replicate (fromIntegral from * pageSize) 0
            memory <- newIORef mem
            return MemoryInstance {
                lim,
                memory
            }

initialize :: ModuleInstance -> Module -> Store -> IO (Either String Store)
initialize inst Module {elems, datas, start} store = do
    checkedMems <- Monad.foldM checkData (Right store) datas
    checkedTables <- Monad.foldM checkElem checkedMems elems
    storeWithTables <- Monad.foldM initElem checkedMems elems
    storeWithMems <- Monad.foldM initData storeWithTables datas
    case storeWithMems of
        Right st -> do
            case start of
                Just (StartFunction idx) -> do
                    let funInst = funcInstances store ! (funcaddrs inst ! fromIntegral idx)
                    mainRes <- eval st funInst []
                    case mainRes of
                        Just [] -> return $ Right st
                        _ -> return $ Left "Start function terminated with trap"
                Nothing -> return $ Right st
        Left reason -> return $ Left reason
    where
        checkElem :: Either String Store -> ElemSegment -> IO (Either String Store)
        checkElem (Left err) _ = return $ Left err
        checkElem (Right st) ElemSegment {tableIndex, offset, funcIndexes} = do
            VI32 val <- evalConstExpr inst st offset
            let from = fromIntegral val
            let funcs = map ((funcaddrs inst !) . fromIntegral) funcIndexes
            let idx = tableaddrs inst ! fromIntegral tableIndex
            let last = from + length funcs
            let TableInstance lim elems = tableInstances st ! idx
            let len = Vector.length elems
            if last > len
            then return $ Left "elements segment does not fit"
            else return $ Right st

        initElem :: Either String Store -> ElemSegment -> IO (Either String Store)
        initElem (Left err) _ = return $ Left err
        initElem (Right st) ElemSegment {tableIndex, offset, funcIndexes} = do
            VI32 val <- evalConstExpr inst st offset
            let from = fromIntegral val
            let funcs = map ((funcaddrs inst !) . fromIntegral) funcIndexes
            let idx = tableaddrs inst ! fromIntegral tableIndex
            let last = from + length funcs
            let TableInstance lim elems = tableInstances st ! idx
            let len = Vector.length elems
            if last > len
            then return $ Left "elements segment does not fit"
            else do
                let table = TableInstance lim (elems // zip [from..] (map Just funcs))
                return $ Right st { tableInstances = tableInstances st Vector.// [(idx, table)] }

        checkData :: Either String Store -> DataSegment -> IO (Either String Store)
        checkData (Left err) _ = return $ Left err
        checkData (Right st) DataSegment {memIndex, offset, chunk} = do
            VI32 val <- evalConstExpr inst st offset
            let from = fromIntegral val
            let idx = memaddrs inst ! fromIntegral memIndex
            let last = from + (fromIntegral $ LBS.length chunk)
            let MemoryInstance _ memory = memInstances st ! idx
            mem <- readIORef memory
            let len = IOVector.length mem
            if last > len
            then return $ Left "data segment does not fit"
            else return $ Right st

        initData :: Either String Store -> DataSegment -> IO (Either String Store)
        initData (Left err) _ = return $ Left err
        initData (Right st) DataSegment {memIndex, offset, chunk} = do
            VI32 val <- evalConstExpr inst st offset
            let from = fromIntegral val
            let idx = memaddrs inst ! fromIntegral memIndex
            let last = from + (fromIntegral $ LBS.length chunk)
            let MemoryInstance _ memory = memInstances st ! idx
            mem <- readIORef memory
            mapM_ (\(i,b) -> IOVector.write mem i b) $ zip [from..] $ LBS.unpack chunk
            return $ Right st

instantiate :: Store -> Imports -> Module -> IO (Either String (ModuleInstance, Store))
instantiate st imps m =
    case calcInstance st imps m of
        Left err -> return $ Left err
        Right inst -> do
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
            return $ (,) inst <$> st'

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

eval :: Store -> FunctionInstance -> [Value] -> IO (Maybe [Value])
eval store FunctionInstance { funcType, moduleInstance, code = Function { localTypes, body} } args = do
    let checkedArgs = zipWith checkValType (params funcType) args
    let initialContext = EvalCtx {
            locals = Vector.fromList $ checkedArgs ++ map initLocal localTypes,
            labels = [Label $ results funcType],
            stack = []
        }
    res <- go initialContext body
    case res of
        Done ctx -> return $ Just $ reverse $ stack ctx
        ReturnFn r -> return $ Just r
        Break 0 r _ -> return $ Just $ reverse r
        Break _ _ _ -> error "Break is out of range"
        Trap -> return Nothing
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
                Done ctx'@EvalCtx{ labels = (_:rest) } -> return $ Done ctx' { labels = rest }
                command -> return command
        step ctx loop@(Loop resType expr) = do
            res <- go ctx { labels = Label resType : labels ctx } expr
            case res of
                Break 0 r EvalCtx{ locals = ls } -> step ctx { locals = ls, stack = r ++ stack ctx } loop
                Break n r ctx' -> return $ Break (n - 1) r ctx'
                Done ctx'@EvalCtx{ labels = (_:rest) } -> return $ Done ctx' { labels = rest }
                command -> return command
        step ctx@EvalCtx{ stack = (VI32 v): rest } (If resType true false) = do
            let expr = if v /= 0 then true else false
            res <- go ctx { labels = Label resType : labels ctx, stack = rest } expr
            case res of
                Break 0 r EvalCtx{ locals = ls } -> return $ Done ctx { locals = ls, stack = r ++ rest }
                Break n r ctx' -> return $ Break (n - 1) r ctx'
                Done ctx'@EvalCtx{ labels = (_:rest) } -> return $ Done ctx' { labels = rest }
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
            let funInst = funcInstances store ! (funcaddrs moduleInstance ! fromIntegral fun)
            let ft = Language.Wasm.Interpreter.funcType funInst 
            let args = params ft
            res <- eval store funInst (zipWith checkValType args $ reverse $ take (length args) $ stack ctx)
            case res of
                Just res -> return $ Done ctx { stack = reverse res ++ (drop (length args) $ stack ctx) }
                Nothing -> return Trap
        step ctx@EvalCtx{ stack = (VI32 v): rest } (CallIndirect typeIdx) = do
            let funcType = funcTypes moduleInstance ! fromIntegral typeIdx
            let TableInstance { elements } = tableInstances store ! (tableaddrs moduleInstance ! 0)
            let funcAddr = elements !? fromIntegral v
            case funcAddr of
                Just (Just addr) -> do
                    let funInst = funcInstances store ! addr
                    let args = params $ Language.Wasm.Interpreter.funcType funInst
                    if length args > length rest
                    then return Trap
                    else do
                        res <- eval store funInst (zipWith checkValType args $ reverse $ take (length args) rest)
                        case res of
                            Just res -> return $ Done ctx { stack = reverse res ++ (drop (length args) rest) }
                            Nothing -> return Trap
                _ -> return Trap
        step ctx@EvalCtx{ stack = (_:rest) } Drop = return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 test:val2:val1:rest) } Select =
            if test == 0
            then return $ Done ctx { stack = val2 : rest }
            else return $ Done ctx { stack = val1 : rest }
        step ctx (GetLocal i) = return $ Done ctx { stack = (locals ctx ! fromIntegral i) : stack ctx }
        step ctx@EvalCtx{ stack = (v:rest) } (SetLocal i) =
            return $ Done ctx { stack = rest, locals = locals ctx // [(fromIntegral i, v)] }
        step ctx@EvalCtx{ locals = ls, stack = (v:rest) } (TeeLocal i) =
            return $ Done ctx {
                stack = v : rest,
                locals = locals ctx // [(fromIntegral i, v)]
            }
        step ctx (GetGlobal i) = do
            let globalInst = globalInstances store ! (globaladdrs moduleInstance ! fromIntegral i)
            val <- case globalInst of
                GIConst _ v -> return v
                GIMut _ ref -> readIORef ref
            return $ Done ctx { stack = val : stack ctx }
        step ctx@EvalCtx{ stack = (v:rest) } (SetGlobal i) = do
            let globalInst = globalInstances store ! (globaladdrs moduleInstance ! fromIntegral i)
            case globalInst of
                GIConst _ v -> error "Attempt of mutation of constant global"
                GIMut _ ref -> writeIORef ref v
            return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            if addr + 4 > IOVector.length memory
            then return Trap
            else do
                val <- sum <$> mapM readByte [0..3]
                return $ Done ctx { stack = VI32 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            if addr + 8 > IOVector.length memory
            then return Trap
            else do
                val <- sum <$> mapM readByte [0..7]
                return $ Done ctx { stack = VI64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (F32Load MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            if addr + 4 > IOVector.length memory
            then return Trap
            else do
                val <- wordToFloat . sum <$> mapM readByte [0..3]
                return $ Done ctx { stack = VF32 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (F64Load MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            if addr + 8 > IOVector.length memory
            then return Trap
            else do
                val <- wordToDouble . sum <$> mapM readByte [0..7]
                return $ Done ctx { stack = VF64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load8U MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            if addr + 1 > IOVector.length memory
            then return Trap
            else do
                byte <- IOVector.read memory addr
                return $ Done ctx { stack = VI32 (fromIntegral byte) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load8S MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            if addr + 4 > IOVector.length memory
            then return Trap
            else do
                byte <- IOVector.read memory addr
                let val = asWord32 $ if byte >= 128 then -1 * fromIntegral (0xFF - byte + 1) else fromIntegral byte
                return $ Done ctx { stack = VI32 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load16U MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            if addr + 2 > IOVector.length memory
            then return Trap
            else do
                val <- sum <$> mapM readByte [0..1]
                return $ Done ctx { stack = VI32 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Load16S MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ (fromIntegral byte :: Word32) `shiftL` (idx * 8)
            if addr + 2 > IOVector.length memory
            then return Trap
            else do
                val <- sum <$> mapM readByte [0..1]
                let signed = asWord32 $ if val >= 2 ^ 15 then -1 * fromIntegral (0xFFFF - val + 1) else fromIntegral val
                return $ Done ctx { stack = VI32 signed : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load8U MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            if addr + 1 > IOVector.length memory
            then return Trap
            else do
                byte <- IOVector.read memory addr
                return $ Done ctx { stack = VI64 (fromIntegral byte) : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load8S MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            if addr + 1 > IOVector.length memory
            then return Trap
            else do
                byte <- IOVector.read memory addr
                let val = asWord64 $ if byte >= 128 then -1 * fromIntegral (0xFF - byte + 1) else fromIntegral byte
                return $ Done ctx { stack = VI64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load16U MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            if addr + 2 > IOVector.length memory
            then return Trap
            else do
                val <- sum <$> mapM readByte [0..1]
                return $ Done ctx { stack = VI64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load16S MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ (fromIntegral byte :: Word32) `shiftL` (idx * 8)
            if addr + 2 > IOVector.length memory
            then return Trap
            else do
                val <- sum <$> mapM readByte [0..1]
                let signed = asWord64 $ if val >= 2 ^ 15 then -1 * fromIntegral (0xFFFF - val + 1) else fromIntegral val
                return $ Done ctx { stack = VI64 signed : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load32U MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            if addr + 4 > IOVector.length memory
            then return Trap
            else do
                val <- sum <$> mapM readByte [0..3]
                return $ Done ctx { stack = VI64 val : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I64Load32S MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ v + fromIntegral offset
            let readByte idx = do
                    byte <- IOVector.read memory $ addr + idx
                    return $ (fromIntegral byte :: Word32) `shiftL` (idx * 8)
            if addr + 4 > IOVector.length memory
            then return Trap
            else do
                val <- sum <$> mapM readByte [0..3]
                let signed = asWord64 $ fromIntegral $ asInt32 val
                return $ Done ctx { stack = VI64 signed : rest }
        step ctx@EvalCtx{ stack = (VI32 v:VI32 va:rest) } (I32Store MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            if addr + 4 > IOVector.length memory
            then return Trap
            else do
                mapM_ writeByte [0..3]
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI64 v:VI32 va:rest) } (I64Store MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            if addr + 8 > IOVector.length memory
            then return Trap
            else do
                mapM_ writeByte [0..7]
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VF32 f:VI32 va:rest) } (F32Store MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let v = floatToWord f
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            if addr + 4 > IOVector.length memory
            then return Trap
            else do
                mapM_ writeByte [0..3]
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VF64 f:VI32 va:rest) } (F64Store MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let v = doubleToWord f
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            if addr + 8 > IOVector.length memory
            then return Trap
            else do
                mapM_ writeByte [0..7]
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 v:VI32 va:rest) } (I32Store8 MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            if addr + 1 > IOVector.length memory
            then return Trap
            else do
                mapM_ writeByte [0]
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 v:VI32 va:rest) } (I32Store16 MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            if addr + 2 > IOVector.length memory
            then return Trap
            else do
                mapM_ writeByte [0, 1]
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI64 v:VI32 va:rest) } (I64Store8 MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            if addr + 1 > IOVector.length memory
            then return Trap
            else do
                mapM_ writeByte [0]
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI64 v:VI32 va:rest) } (I64Store16 MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            if addr + 2 > IOVector.length memory
            then return Trap
            else do
                mapM_ writeByte [0, 1]
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI64 v:VI32 va:rest) } (I64Store32 MemArg { offset }) = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    IOVector.write memory (addr + idx) byte
            if addr + 4 > IOVector.length memory
            then return Trap
            else do
                mapM_ writeByte [0..3]
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = st } CurrentMemory = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let size = fromIntegral $ IOVector.length memory `div` pageSize
            return $ Done ctx { stack = VI32 size : st }
        step ctx@EvalCtx{ stack = (VI32 n:rest) } GrowMemory = do
            let MemoryInstance { lim = limit@(Limit _ maxLen), memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let size = fromIntegral $ IOVector.length memory `quot` pageSize
            let growTo = size + fromIntegral n
            result <- (
                    if fromMaybe True ((growTo <=) . fromIntegral <$> maxLen) && growTo <= 0xFFFF
                    then do
                        mem' <- IOVector.grow memory $ fromIntegral n * pageSize
                        writeIORef memoryRef mem'
                        return size
                    else return $ -1
                )
            return $ Done ctx { stack = VI32 (asWord32 $ fromIntegral result) : rest }
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
            if v2 == 0
            then return Trap
            else return $ Done ctx { stack = VI32 (v1 `quot` v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IDivS) =
            if v2 == 0 || (v1 == 0x80000000 && v2 == 0xFFFFFFFF)
            then return Trap
            else return $ Done ctx { stack = VI32 (asWord32 $ asInt32 v1 `quot` asInt32 v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IRemU) =
            if v2 == 0
            then return Trap
            else return $ Done ctx { stack = VI32 (v1 `rem` v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IRemS) =
            if v2 == 0
            then return Trap
            else return $ Done ctx { stack = VI32 (asWord32 $ asInt32 v1 `rem` asInt32 v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IAnd) =
            return $ Done ctx { stack = VI32 (v1 .&. v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IOr) =
            return $ Done ctx { stack = VI32 (v1 .|. v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IXor) =
            return $ Done ctx { stack = VI32 (v1 `xor` v2) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IShl) =
            return $ Done ctx { stack = VI32 (v1 `shiftL` (fromIntegral v2 `rem` 32)) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IShrU) =
            return $ Done ctx { stack = VI32 (v1 `shiftR` (fromIntegral v2 `rem` 32)) : rest }
        step ctx@EvalCtx{ stack = (VI32 v2:VI32 v1:rest) } (IBinOp BS32 IShrS) =
            return $ Done ctx { stack = VI32 (asWord32 $ asInt32 v1 `shiftR` (fromIntegral v2 `rem` 32)) : rest }
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
            return $ Done ctx { stack = VI64 (v1 + v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 ISub) =
            return $ Done ctx { stack = VI64 (v1 - v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IMul) =
            return $ Done ctx { stack = VI64 (v1 * v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IDivU) =
            if v2 == 0
            then return Trap
            else return $ Done ctx { stack = VI64 (v1 `quot` v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IDivS) =
            if v2 == 0 || (v1 == 0x8000000000000000 && v2 == 0xFFFFFFFFFFFFFFFF)
            then return Trap
            else return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 `quot` asInt64 v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IRemU) =
            if v2 == 0
            then return Trap
            else return $ Done ctx { stack = VI64 (v1 `rem` v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IRemS) =
            if v2 == 0
            then return Trap
            else return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 `rem` asInt64 v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IAnd) =
            return $ Done ctx { stack = VI64 (v1 .&. v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IOr) =
            return $ Done ctx { stack = VI64 (v1 .|. v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IXor) =
            return $ Done ctx { stack = VI64 (v1 `xor` v2) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IShl) =
            return $ Done ctx { stack = VI64 (v1 `shiftL` (fromIntegral v2 `rem` 64)) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IShrU) =
            return $ Done ctx { stack = VI64 (v1 `shiftR` (fromIntegral v2 `rem` 64)) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IShrS) =
            return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 `shiftR` (fromIntegral v2 `rem` 64)) : rest }
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
            return $ Done ctx { stack = VI64 (fromIntegral $ countLeadingZeros v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (IUnOp BS64 ICtz) =
            return $ Done ctx { stack = VI64 (fromIntegral $ countTrailingZeros v) : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (IUnOp BS64 IPopcnt) =
            return $ Done ctx { stack = VI64 (fromIntegral $ popCount v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FAbs) =
            return $ Done ctx { stack = VF32 (abs v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FNeg) =
            return $ Done ctx { stack = VF32 (negate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FCeil) =
            return $ Done ctx { stack = VF32 (floatCeil v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FFloor) =
            return $ Done ctx { stack = VF32 (floatFloor v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FTrunc) =
            return $ Done ctx { stack = VF32 (floatTrunc v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FNearest) =
            return $ Done ctx { stack = VF32 (nearest v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (FUnOp BS32 FSqrt) =
            return $ Done ctx { stack = VF32 (sqrt v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FAbs) =
            return $ Done ctx { stack = VF64 (abs v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FNeg) =
            return $ Done ctx { stack = VF64 (negate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FCeil) =
            return $ Done ctx { stack = VF64 (doubleCeil v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FFloor) =
            return $ Done ctx { stack = VF64 (doubleFloor v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (FUnOp BS64 FTrunc) =
            return $ Done ctx { stack = VF64 (doubleTrunc v) : rest }
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
            return $ Done ctx { stack = VF32 (zeroAwareMin v1 v2) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FBinOp BS32 FMax) =
            return $ Done ctx { stack = VF32 (zeroAwareMax v1 v2) : rest }
        step ctx@EvalCtx{ stack = (VF32 v2:VF32 v1:rest) } (FBinOp BS32 FCopySign) =
            return $ Done ctx { stack = VF32 (copySign v1 v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FAdd) =
            return $ Done ctx { stack = VF64 (v1 + v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FSub) =
            return $ Done ctx { stack = VF64 (v1 - v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FMul) =
            return $ Done ctx { stack = VF64 (v1 * v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FDiv) =
            return $ Done ctx { stack = VF64 (v1 / v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FMin) =
            return $ Done ctx { stack = VF64 (zeroAwareMin v1 v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FMax) =
            return $ Done ctx { stack = VF64 (zeroAwareMax v1 v2) : rest }
        step ctx@EvalCtx{ stack = (VF64 v2:VF64 v1:rest) } (FBinOp BS64 FCopySign) =
            return $ Done ctx { stack = VF64 (copySign v1 v2) : rest }
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
            if isNaN v
            then return Trap
            else return $ Done ctx { stack = VI32 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFU BS32 BS64) =
            if isNaN v
            then return Trap
            else return $ Done ctx { stack = VI32 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFU BS64 BS32) =
            if isNaN v
            then return Trap
            else return $ Done ctx { stack = VI64 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFU BS64 BS64) =
            if isNaN v
            then return Trap
            else return $ Done ctx { stack = VI64 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFS BS32 BS32) =
            if isNaN v
            then return Trap
            else return $ Done ctx { stack = VI32 (asWord32 $ truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFS BS32 BS64) =
            if isNaN v
            then return Trap
            else return $ Done ctx { stack = VI32 (asWord32 $ truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFS BS64 BS32) =
            if isNaN v
            then return Trap
            else return $ Done ctx { stack = VI64 (asWord64 $ truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFS BS64 BS64) =
            if isNaN v
            then return Trap
            else return $ Done ctx { stack = VI64 (asWord64 $ truncate v) : rest }
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
        step EvalCtx{ stack } instr = error $ "Error during evaluation of instruction: " ++ show instr ++ ". Stack " ++ show stack
eval _ HostInstance { funcType, hostCode } args = Just <$> hostCode args

invoke :: Store -> Address -> [Value] -> IO (Maybe [Value])
invoke st funcIdx = eval st $ funcInstances st ! funcIdx

invokeExport :: Store -> ModuleInstance -> TL.Text -> [Value] -> IO (Maybe [Value])
invokeExport st ModuleInstance { exports } name args =
    case Vector.find (\(ExportInstance n _) -> n == name) exports of
        Just (ExportInstance _ (ExternFunction addr)) -> invoke st addr args
        _ -> error $ "Function with name " ++ show name ++ " was not found in module's exports"

getGlobalValueByName :: Store -> ModuleInstance -> TL.Text -> IO Value
getGlobalValueByName store ModuleInstance { exports } name =
    case Vector.find (\(ExportInstance n _) -> n == name) exports of
        Just (ExportInstance _ (ExternGlobal addr)) ->
            let globalInst = globalInstances store ! addr in
            case globalInst of
                GIConst _ v -> return v
                GIMut _ ref -> readIORef ref
        _ -> error $ "Function with name " ++ show name ++ " was not found in module's exports"