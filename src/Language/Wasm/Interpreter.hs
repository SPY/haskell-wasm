{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Language.Wasm.Interpreter (
    Value(..),
    Store,
    ModuleInstance(..),
    ExternalValue(..),
    ExportInstance(..),
    GlobalInstance(..),
    MemoryInstance(..),
    MemoryStore,
    Imports,
    HostItem(..), 
    Address,
    instantiate,
    invoke,
    invokeExport,
    getGlobalValueByName,
    emptyStore,
    emptyImports,
    makeHostModule,
    makeMutGlobal,
    makeConstGlobal, 
    getMemory
) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing)

import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as Vector
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVector
import qualified Data.Primitive.ByteArray as ByteArray
import qualified Data.Primitive.Types as Primitive
import qualified Control.Monad.Primitive as Primitive
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int32, Int64)
import Numeric.Natural (Natural)
import qualified Control.Monad as Monad
import Data.Bits (
        Bits,
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
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Control.Monad.State as State
import Control.Monad.IO.Class (liftIO)

import Language.Wasm.Structure as Struct
import Language.Wasm.Validate as Valid
import Language.Wasm.FloatUtils (
        wordToFloat,
        floatToWord,
        wordToDouble,
        doubleToWord
    )

import Debug.Trace as Debug

data Value =
    VI32 Word32
    | VI64 Word64
    | VF32 Float
    | VF64 Double
    | RF (Maybe Natural)
    | RE (Maybe Natural)
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

type TableStore = IORef (IOVector (Maybe Address))

data TableInstance = TableInstance {
    t :: TableType,
    items :: TableStore
}

type MemoryStore = ByteArray.MutableByteArray (Primitive.PrimState IO)

data MemoryInstance = MemoryInstance {
    lim :: Limit,
    memory :: IORef MemoryStore
}

data GlobalInstance = GIConst ValueType Value | GIMut ValueType (IORef Value)

makeMutGlobal :: Value -> IO GlobalInstance
makeMutGlobal val = GIMut (getValueType val) <$> newIORef val

makeConstGlobal :: Value -> GlobalInstance
makeConstGlobal val = GIConst (getValueType val) val

getValueType :: Value -> ValueType
getValueType (VI32 _) = I32
getValueType (VI64 _) = I64
getValueType (VF32 _) = F32
getValueType (VF64 _) = F64
getValueType (RF _) = Func
genValueType (RE _) = Extern

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

data ElemInstance = ElemInstance {
        eiMode :: ElemMode,
        eiType :: ElemType,
        eiItems :: Vector Value,
        isDropped :: IORef Bool
    }

isDeclarative :: ElemMode -> Bool 
isDeclarative Declarative = True
isDeclarative _           = False

data DataInstance = DataInstance {
        dMode :: DataMode,
        isDropped :: IORef Bool,
        bytes :: LBS.ByteString
    }

data Store = Store {
    funcInstances :: Vector FunctionInstance,
    tableInstances :: Vector TableInstance,
    memInstances :: Vector MemoryInstance,
    globalInstances :: Vector GlobalInstance,
    elemInstances :: Vector ElemInstance,
    dataInstances :: Vector DataInstance
}

emptyStore :: Store
emptyStore = Store {
    funcInstances = Vector.empty,
    tableInstances = Vector.empty,
    memInstances = Vector.empty,
    globalInstances = Vector.empty,
    elemInstances = Vector.empty,
    dataInstances = Vector.empty
}

type HostFunction = [Value] -> IO [Value]

data HostItem
    = HostFunction FuncType HostFunction
    | HostGlobal GlobalInstance
    | HostMemory Limit
    | HostTable Limit

makeHostModule :: Store -> [(TL.Text, HostItem)] -> IO (Store, ModuleInstance)
makeHostModule st items = do
    (st, emptyModInstance)
        |> makeHostFunctions
        |> makeHostGlobals
        |> makeHostMems
        >>= makeHostTables
    where
        (|>) = flip ($)

        makeHostFunctions :: (Store, ModuleInstance) -> (Store, ModuleInstance)
        makeHostFunctions (st, inst) =
            let funcLen = Vector.length $ funcInstances st in
            let (names, types, instances) = unzip3 [(name, t, HostInstance t c) | (name, (HostFunction t c)) <- items] in
            let exps = Vector.fromList $ zipWith (\name i -> ExportInstance name (ExternFunction i)) names [funcLen..] in
            let inst' = inst {
                    funcTypes = Vector.fromList types,
                    funcaddrs = Vector.fromList [funcLen..funcLen + length instances - 1],
                    exports = Language.Wasm.Interpreter.exports inst <> exps
                }
            in
            let st' = st { funcInstances = funcInstances st <> Vector.fromList instances } in
            (st', inst')
        
        makeHostGlobals :: (Store, ModuleInstance) -> (Store, ModuleInstance)
        makeHostGlobals (st, inst) =
            let globLen = Vector.length $ globalInstances st in
            let (names, instances) = unzip [(name, g) | (name, (HostGlobal g)) <- items] in
            let exps = Vector.fromList $ zipWith (\name i -> ExportInstance name (ExternGlobal i)) names [globLen..] in
            let inst' = inst {
                    globaladdrs = Vector.fromList [globLen..globLen + length instances - 1],
                    exports = Language.Wasm.Interpreter.exports inst <> exps
                }
            in
            let st' = st { globalInstances = globalInstances st <> Vector.fromList instances } in
            (st', inst')
            
        makeHostMems :: (Store, ModuleInstance) -> IO (Store, ModuleInstance)
        makeHostMems (st, inst) = do
            let memLen = Vector.length $ memInstances st
            let (names, limits) = unzip [(name, Memory lim) | (name, (HostMemory lim)) <- items]
            instances <- allocMems limits
            let exps = Vector.fromList $ zipWith (\name i -> ExportInstance name (ExternMemory i)) names [memLen..]
            let inst' = inst {
                    memaddrs = Vector.fromList [memLen..memLen + length instances - 1],
                    exports = Language.Wasm.Interpreter.exports inst <> exps
                }
            let st' = st { memInstances = memInstances st <> instances }
            return (st', inst')
            
        makeHostTables :: (Store, ModuleInstance) -> IO (Store, ModuleInstance)
        makeHostTables (st, inst) = do
            let tableLen = Vector.length $ tableInstances st
            let (names, tables) = unzip [(name, Table (TableType lim FuncRef)) | (name, HostTable lim) <- items]
            instances <- allocTables tables
            let exps = Vector.fromList $ zipWith (\name i -> ExportInstance name (ExternTable i)) names [tableLen..]
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
    elemaddrs :: Vector Address,
    dataaddrs :: Vector Address,
    exports :: Vector ExportInstance
} deriving (Eq, Show)

emptyModInstance :: ModuleInstance
emptyModInstance = ModuleInstance {
    funcTypes = Vector.empty,
    funcaddrs = Vector.empty,
    tableaddrs = Vector.empty,
    memaddrs = Vector.empty,
    globaladdrs = Vector.empty,
    elemaddrs = Vector.empty,
    dataaddrs = Vector.empty,
    exports = Vector.empty
}

calcInstance :: Store -> Imports -> Module -> Initialize ModuleInstance
calcInstance (Store fs ts ms gs es ds) imps mod = do
    let Module {functions, types, tables, mems, globals, exports, imports, elems, datas} = mod
    let funLen = length fs
    let tableLen = length ts
    let memLen = length ms
    let globalLen = length gs
    let elemLen = length es
    let dataLen = length ds
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
        elemaddrs = Vector.fromList [elemLen..elemLen + length elems - 1],
        dataaddrs = Vector.fromList [dataLen..dataLen + length datas - 1],
        exports = Vector.fromList $ map refExport exports
    }
    where
        getImpIdx :: Import -> Initialize ExternalValue
        getImpIdx (Import m n _) =
            case Map.lookup (m, n) imps of
                Just idx -> return idx
                Nothing -> throwError $ "Cannot find import from module " ++ show m ++ " with name " ++ show n

        checkImportType :: Import -> Initialize ExternalValue
        checkImportType imp@(Import _ _ (ImportFunc typeIdx)) = do
            idx <- getImpIdx imp
            funcAddr <- case idx of
                ExternFunction funcAddr -> return funcAddr
                other -> throwError "incompatible import type"
            let expectedType = types mod !! fromIntegral typeIdx
            let actualType = Language.Wasm.Interpreter.funcType $ fs ! funcAddr
            if expectedType == actualType
            then return idx
            else throwError "incompatible import type"
        checkImportType imp@(Import _ _ (ImportGlobal globalType)) = do
            let err = throwError "incompatible import type"
            idx <- getImpIdx imp
            globalAddr <- case idx of
                ExternGlobal globalAddr -> return globalAddr
                _ -> err
            let globalInst = gs ! globalAddr
            let typesMatch = case (globalType, globalInst) of
                    (Const vt, GIConst vt' _) -> vt == vt'
                    (Mut vt, GIMut vt' _) -> vt == vt'
                    _ -> False
            if typesMatch then return idx else err
        checkImportType imp@(Import _ _ (ImportMemory limit)) = do
            idx <- getImpIdx imp
            memAddr <- case idx of
                ExternMemory memAddr -> return memAddr
                _ -> throwError "incompatible import type"
            let MemoryInstance { lim = Limit _ limMax, memory = mem } = ms ! memAddr
            size <- liftIO $ (`quot` pageSize) <$> (readIORef mem >>= ByteArray.getSizeofMutableByteArray)
            if limitMatch (Limit (fromIntegral size) limMax) limit
            then return idx
            else throwError "incompatible import type"
        checkImportType imp@(Import _ _ (ImportTable (TableType limit et))) = do
            idx <- getImpIdx imp
            tableAddr <- case idx of
                ExternTable tableAddr -> return tableAddr
                _ -> throwError "incompatible import type"
            let TableInstance { t = TableType lim et' } = ts ! tableAddr
            if limitMatch lim limit && et == et'
            then return idx
            else throwError "incompatible import type"
    
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
evalConstExpr :: ModuleInstance -> Store -> Expression -> IO Value
evalConstExpr _ _ [I32Const v] = return $ VI32 v
evalConstExpr _ _ [I64Const v] = return $ VI64 v
evalConstExpr _ _ [F32Const v] = return $ VF32 v
evalConstExpr _ _ [F64Const v] = return $ VF64 v
evalConstExpr _ _ [RefNull FuncRef] = return $ RF Nothing
evalConstExpr _ _ [RefNull ExternRef] = return $ RE Nothing
evalConstExpr inst _ [RefFunc idx] = return $ RF $ Just $ fromIntegral $ funcaddrs inst ! fromIntegral idx
evalConstExpr inst store [GetGlobal i] = getGlobalValue inst store i
evalConstExpr _ _ instrs = error $ "Global initializer contains unsupported instructions: " ++ show instrs

allocAndInitGlobals :: ModuleInstance -> Store -> [Global] -> IO (Vector GlobalInstance)
allocAndInitGlobals inst store globs = Vector.fromList <$> mapM allocGlob globs
    where
        runIniter :: Expression -> IO Value
        -- the spec says get global can ref only imported globals
        -- only they are in store for this moment
        runIniter = evalConstExpr inst store

        allocGlob :: Global -> IO GlobalInstance
        allocGlob (Global (Const vt) initer) = GIConst vt <$> runIniter initer
        allocGlob (Global (Mut vt) initer) = do
            val <- runIniter initer
            GIMut vt <$> newIORef val

allocTables :: [Table] -> IO (Vector TableInstance)
allocTables = fmap Vector.fromList . mapM allocTable
    where
        allocTable :: Table -> IO TableInstance
        allocTable (Table t@(TableType lim@(Limit from to) _)) =
            let elements = MVector.replicate (fromIntegral from) Nothing in
            TableInstance t <$> (elements >>= newIORef)

defaultBudget :: Natural
defaultBudget = 300

pageSize :: Int
pageSize = 64 * 1024

allocMems :: [Memory] -> IO (Vector MemoryInstance)
allocMems mems = Vector.fromList <$> mapM allocMem mems
    where
        allocMem :: Memory -> IO MemoryInstance
        allocMem (Memory lim@(Limit from to)) = do
            let size = fromIntegral from * pageSize
            mem <- ByteArray.newByteArray size
            ByteArray.setByteArray @Word64 mem 0 (size `div` 8) 0
            memory <- newIORef mem
            return MemoryInstance {
                lim,
                memory
            }

allocElems :: ModuleInstance -> Store -> [ElemSegment] -> IO (Vector ElemInstance)
allocElems inst st = fmap Vector.fromList . mapM allocElem
    where
        allocElem :: ElemSegment -> IO ElemInstance
        allocElem (ElemSegment t mode refs) = do
            indexes <- flip mapM refs $ \refExpr -> do
                evalConstExpr inst st refExpr
            ElemInstance mode t (Vector.fromList indexes)
                <$> newIORef False -- is dropped

allocDatas :: [DataSegment] -> IO (Vector DataInstance)
allocDatas datas = Vector.fromList <$> Monad.forM datas (\DataSegment {dataMode, chunk} -> do
    isDropped <- newIORef False
    return $ DataInstance dataMode isDropped chunk)

type Initialize = ExceptT String (State.StateT Store IO)

initialize :: ModuleInstance -> Module -> Initialize ()
initialize inst Module {elems, datas, start} = do
    checkedMems <- mapM checkData datas
    checkedTables <- mapM checkElem $ filter isActiveElem $ zip [0..] elems
    mapM_ initData checkedMems
    mapM_ initElem checkedTables
    st <- State.get
    case start of
        Just (StartFunction idx) -> do
            let funInst = funcInstances st ! (funcaddrs inst ! fromIntegral idx)
            mainRes <- liftIO $ eval defaultBudget st inst funInst []
            case mainRes of
                Just [] -> return ()
                _ -> throwError "Start function terminated with trap"
        Nothing -> return ()
    where
        isActiveElem :: (Int, ElemSegment) -> Bool
        isActiveElem (_, ElemSegment _ (Active _ _) _) = True
        isActiveElem _ = False

        checkElem :: (Int, ElemSegment) -> Initialize (Address, Address, Int, [Maybe Address])
        checkElem (elemN, ElemSegment {elemType, mode, elements}) = do
            (tableIndex, offset) <- case mode of {
                Active idx off -> return (idx, off);
                _ -> throwError "only active mode element can be initialized"
            }
            st <- State.get
            VI32 val <- liftIO $ evalConstExpr inst st offset
            let from = fromIntegral val
            refs <- liftIO $ mapM (evalConstExpr inst st) elements
            let toStoreIndex ref = case ref of
                    RF idx -> fromIntegral <$> idx
                    RE idx -> fromIntegral <$> idx
            let funcs = map toStoreIndex refs
            let idx = tableaddrs inst ! fromIntegral tableIndex
            return (idx, elemaddrs inst ! elemN, from, funcs)

        initElem :: (Address, Address, Int, [Maybe Address]) -> Initialize ()
        initElem (tableIdx, elemIdx, from, funcs) = do
            Store {tableInstances, elemInstances} <- State.get
            elems <- liftIO $ readIORef $ items $ tableInstances ! tableIdx
            if from + length funcs > MVector.length elems
            then throwError "out of bounds table access"
            else do
                let ElemInstance {isDropped} = elemInstances ! elemIdx
                liftIO $ writeIORef isDropped True
                Monad.forM_ (zip [from..] funcs) $ uncurry $ MVector.unsafeWrite elems

        checkData :: DataSegment -> Initialize (Maybe (Int, MemoryStore, LBS.ByteString))
        checkData DataSegment {dataMode = ActiveData memIndex offset, chunk} = do
            st <- State.get
            VI32 val <- liftIO $ evalConstExpr inst st offset
            let from = fromIntegral val
            let idx = memaddrs inst ! fromIntegral memIndex
            let MemoryInstance _ memory = memInstances st ! idx
            mem <- liftIO $ readIORef memory
            return $ Just (from, mem, chunk)
        checkData DataSegment {dataMode = PassiveData, chunk} =
            return Nothing
        
        initData :: Maybe (Int, MemoryStore, LBS.ByteString) -> Initialize ()
        initData Nothing = return ()
        initData (Just (from, mem, chunk)) = do
            let last = from + (fromIntegral $ LBS.length chunk)
            len <- ByteArray.getSizeofMutableByteArray mem
            Monad.when (last > len) $ throwError "out of bounds memory access"
            mapM_ (\(i,b) -> ByteArray.writeByteArray mem i b) $ zip [from..] $ LBS.unpack chunk

instantiate :: Store -> Imports -> Valid.ValidModule -> IO (Either String ModuleInstance, Store)
instantiate st imps mod = flip State.runStateT st $ runExceptT $ do
    let m = Valid.getModule mod
    inst <- calcInstance st imps m
    let functions = funcInstances st <> allocFunctions inst (Struct.functions m)
    globals <- liftIO $ (globalInstances st <>) <$> allocAndInitGlobals inst st (Struct.globals m)
    tables <- (tableInstances st <>) <$> liftIO (allocTables (Struct.tables m))
    mems <- liftIO $ (memInstances st <>) <$> allocMems (Struct.mems m)
    elems <- liftIO $ (elemInstances st <>) <$> allocElems inst st (Struct.elems m)
    datas <- liftIO $ (dataInstances st <>) <$> allocDatas (Struct.datas m)
    State.put $ st {
        funcInstances = functions,
        tableInstances = tables,
        memInstances = mems,
        globalInstances = globals,
        elemInstances = elems,
        dataInstances = datas
    }
    initialize inst m
    return inst

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

eval :: Natural -> Store -> ModuleInstance -> FunctionInstance -> [Value] -> IO (Maybe [Value])
eval 0 _ _ _ _ = return Nothing
eval budget store inst FunctionInstance { funcType, moduleInstance, code = Function { localTypes, body} } args = do
    case sequence $ zipWith checkValType (params funcType) args of
        Just checkedArgs -> do
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
        Nothing -> return Nothing
    where
        checkValType :: ValueType -> Value -> Maybe Value
        checkValType I32 (VI32 v) = Just $ VI32 v
        checkValType I64 (VI64 v) = Just $ VI64 v
        checkValType F32 (VF32 v) = Just $ VF32 v
        checkValType F64 (VF64 v) = Just $ VF64 v
        checkValType Func (RF v)  = Just $ RF v
        checkValType Extern (RE v) = Just $ RE v
        checkValType _   _        = Nothing

        initLocal :: ValueType -> Value
        initLocal I32 = VI32 0
        initLocal I64 = VI64 0
        initLocal F32 = VF32 0
        initLocal F64 = VF64 0

        go :: EvalCtx -> Expression -> IO EvalResult
        go ctx [] = return $ Done ctx
        go ctx (instr:rest) = do
            res <- step ctx instr
            case res of
                Done ctx' -> go ctx' rest
                command -> return command
        
        makeLoadInstr :: (Primitive.Prim i, Bits i, Integral i) => EvalCtx -> Natural -> Int -> ([Value] -> i -> EvalResult) -> IO EvalResult
        makeLoadInstr ctx@EvalCtx{ stack = (VI32 v:rest) } offset byteWidth cont = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral v + fromIntegral offset
            let readByte idx = do
                    byte <- ByteArray.readByteArray @Word8 memory $ addr + idx
                    return $ fromIntegral byte `shiftL` (idx * 8)
            len <- ByteArray.getSizeofMutableByteArray memory
            let isAligned = addr `rem` byteWidth == 0
            if addr + byteWidth > len
            then return Trap
            else (
                    if isAligned
                    then cont rest <$> ByteArray.readByteArray memory (addr `quot` byteWidth)
                    else cont rest . sum <$> mapM readByte [0..byteWidth-1]
                )
        makeLoadInstr _ _ _ _ = error "Incorrect value on top of stack for memory instruction"

        makeStoreInstr :: (Primitive.Prim i, Bits i, Integral i) => EvalCtx -> Natural -> Int -> i -> IO EvalResult
        makeStoreInstr ctx@EvalCtx{ stack = (VI32 va:rest) } offset byteWidth v = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            let addr = fromIntegral $ va + fromIntegral offset
            let writeByte idx = do
                    let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
                    ByteArray.writeByteArray @Word8 memory (addr + idx) byte
            len <- ByteArray.getSizeofMutableByteArray memory
            let isAligned = addr `rem` byteWidth == 0
            let write = if isAligned
                then ByteArray.writeByteArray memory (addr `quot` byteWidth) v
                else mapM_ writeByte [0..byteWidth-1] :: IO ()
            if addr + byteWidth > len
            then return Trap
            else write >> (return $ Done ctx { stack = rest })
        makeStoreInstr _ _ _ _ = error "Incorrect value on top of stack for memory instruction"

        step :: EvalCtx -> Instruction Natural -> IO EvalResult
        step _ Unreachable = return Trap
        step ctx Nop = return $ Done ctx
        step ctx (Block blockType expr) = do
            let FuncType paramType resType = case blockType of
                    Inline Nothing -> FuncType [] []
                    Inline (Just valType) -> FuncType [] [valType]
                    TypeIndex typeIdx -> funcTypes moduleInstance ! fromIntegral typeIdx
            res <- go ctx { labels = Label resType : labels ctx } expr
            case res of
                Break 0 r EvalCtx{ locals = ls } -> return $ Done ctx { locals = ls, stack = r ++ (drop (length paramType) $ stack ctx) }
                Break n r ctx' -> return $ Break (n - 1) r ctx'
                Done ctx'@EvalCtx{ labels = (_:rest) } -> return $ Done ctx' { labels = rest }
                command -> return command
        step ctx loop@(Loop blockType expr) = do
            let resType = case blockType of
                    Inline Nothing -> []
                    Inline (Just valType) -> [valType]
                    TypeIndex typeIdx -> results $ funcTypes moduleInstance ! fromIntegral typeIdx
            res <- go ctx { labels = Label resType : labels ctx } expr
            case res of
                Break 0 r EvalCtx{ locals = ls, stack = st } -> step ctx { locals = ls, stack = st } loop
                Break n r ctx' -> return $ Break (n - 1) r ctx'
                Done ctx'@EvalCtx{ labels = (_:rest) } -> return $ Done ctx' { labels = rest }
                command -> return command
        step ctx@EvalCtx{ stack = (VI32 v): rest } (If blockType true false) = do
            let FuncType paramType resType = case blockType of
                    Inline Nothing -> FuncType [] []
                    Inline (Just valType) -> FuncType [] [valType]
                    TypeIndex typeIdx -> funcTypes moduleInstance ! fromIntegral typeIdx
            let expr = if v /= 0 then true else false
            res <- go ctx { labels = Label resType : labels ctx, stack = rest } expr
            case res of
                Break 0 r EvalCtx{ locals = ls } -> return $ Done ctx { locals = ls, stack = r ++ (drop (length paramType) rest) }
                Break n r ctx' -> return $ Break (n - 1) r ctx'
                Done ctx'@EvalCtx{ labels = (_:rest) } -> return $ Done ctx' { labels = rest }
                command -> return command
        step ctx@EvalCtx{ stack, labels } (Br label) = do
            let idx = fromIntegral label
            let Label resType = labels !! idx
            case sequence $ zipWith checkValType (reverse resType) $ take (length resType) stack of
                Just result -> return $ Break idx result ctx
                Nothing -> return Trap
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
            case sequence $ zipWith checkValType (reverse resType) $ take (length resType) stack of
                Just result -> return $ ReturnFn $ reverse result
                Nothing -> return Trap
        step ctx (Call fun) = do
            let funInst = funcInstances store ! (funcaddrs moduleInstance ! fromIntegral fun)
            let ft = Language.Wasm.Interpreter.funcType funInst 
            let args = params ft
            case sequence $ zipWith checkValType args $ reverse $ take (length args) $ stack ctx of
                Just params -> do
                    res <- eval (budget - 1) store inst funInst params
                    case res of
                        Just res -> return $ Done ctx { stack = reverse res ++ (drop (length args) $ stack ctx) }
                        Nothing -> return Trap
                Nothing -> return Trap
        step ctx@EvalCtx{ stack = (VI32 v): rest } (CallIndirect tableIdx typeIdx) = do
            let funcType = funcTypes moduleInstance ! fromIntegral typeIdx
            let TableInstance { items } = tableInstances store ! (tableaddrs moduleInstance ! fromIntegral tableIdx)
            let pos = fromIntegral v
            funcs <- readIORef items
            if pos >= MVector.length funcs
            then return Trap
            else do
                maybeAddr <- MVector.unsafeRead funcs pos
                let checks = do
                        addr <- maybeAddr
                        let funcInst = funcInstances store ! addr
                        let targetType = Language.Wasm.Interpreter.funcType funcInst
                        Monad.guard $ targetType == funcType
                        let args = params targetType
                        Monad.guard $ length args <= length rest
                        params <- sequence $ zipWith checkValType args $ reverse $ take (length args) rest
                        return (funcInst, params)
                case checks of
                    Just (funcInst, params) -> do
                        res <- eval (budget - 1) store inst funcInst params
                        case res of
                            Just res -> return $ Done ctx { stack = reverse res ++ (drop (length params) rest) }
                            Nothing -> return Trap
                    Nothing -> return Trap
        step ctx@EvalCtx{ stack = st } (RefNull FuncRef) =
            return $ Done ctx { stack = RF Nothing : st }
        step ctx@EvalCtx{ stack = st } (RefNull ExternRef) =
            return $ Done ctx { stack = RE Nothing : st }
        step ctx@EvalCtx{ stack = v:rest } RefIsNull =
            let r = case v of { RE Nothing -> 1; RF Nothing -> 1; _ -> 0 } in
            return $ Done ctx { stack = VI32 r : rest }
        step ctx@EvalCtx{ stack = st } (RefFunc index) =
            return $ Done ctx { stack =  (RF $ Just $ fromIntegral $ funcaddrs inst ! fromIntegral index) : st }
        step ctx@EvalCtx{ stack = (_:rest) } Drop = return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 test:val2:val1:rest) } (Select _) =
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
        step ctx (I32Load MemArg { offset }) =
            makeLoadInstr ctx offset 4 $ (\rest val -> Done ctx { stack = VI32 val : rest })
        step ctx (I64Load MemArg { offset }) =
            makeLoadInstr ctx offset 8 $ (\rest val -> Done ctx { stack = VI64 val : rest })
        step ctx (F32Load MemArg { offset }) =
            makeLoadInstr ctx offset 4 $ (\rest val -> Done ctx { stack = VF32 (wordToFloat val) : rest })
        step ctx (F64Load MemArg { offset }) =
            makeLoadInstr ctx offset 8 $ (\rest val -> Done ctx { stack = VF64 (wordToDouble val) : rest })
        step ctx (I32Load8U MemArg { offset }) =
            makeLoadInstr @Word8 ctx offset 1 $ (\rest val -> Done ctx { stack = VI32 (fromIntegral val) : rest })
        step ctx (I32Load8S MemArg { offset }) =
            makeLoadInstr ctx offset 1 $ (\rest byte ->
                let val = asWord32 $ if (byte :: Word8) >= 128 then -1 * fromIntegral (0xFF - byte + 1) else fromIntegral byte in
                Done ctx { stack = VI32 val : rest })
        step ctx (I32Load16U MemArg { offset }) = do
            makeLoadInstr @Word16 ctx offset 2 $ (\rest val -> Done ctx { stack = VI32 (fromIntegral val) : rest })
        step ctx (I32Load16S MemArg { offset }) =
            makeLoadInstr ctx offset 2 $ (\rest val ->
                let signed = asWord32 $ if (val :: Word16) >= 2 ^ 15 then -1 * fromIntegral (0xFFFF - val + 1) else fromIntegral val in
                Done ctx { stack = VI32 signed : rest })
        step ctx (I64Load8U MemArg { offset }) =
            makeLoadInstr @Word8 ctx offset 1 $ (\rest val -> Done ctx { stack = VI64 (fromIntegral val) : rest })
        step ctx (I64Load8S MemArg { offset }) =
            makeLoadInstr ctx offset 1 $ (\rest byte ->
                let val = asWord64 $ if (byte :: Word8) >= 128 then -1 * fromIntegral (0xFF - byte + 1) else fromIntegral byte in
                Done ctx { stack = VI64 val : rest })
        step ctx (I64Load16U MemArg { offset }) =
            makeLoadInstr @Word16 ctx offset 2 $ (\rest val -> Done ctx { stack = VI64 (fromIntegral val) : rest })
        step ctx (I64Load16S MemArg { offset }) =
            makeLoadInstr ctx offset 2 $ (\rest val ->
                let signed = asWord64 $ if (val :: Word16) >= 2 ^ 15 then -1 * fromIntegral (0xFFFF - val + 1) else fromIntegral val in
                Done ctx { stack = VI64 signed : rest })
        step ctx (I64Load32U MemArg { offset }) =
            makeLoadInstr @Word32 ctx offset 4 $ (\rest val -> Done ctx { stack = VI64 (fromIntegral val) : rest })
        step ctx (I64Load32S MemArg { offset }) =
            makeLoadInstr ctx offset 4 $ (\rest val ->
                let signed = asWord64 $ fromIntegral $ asInt32 val in
                Done ctx { stack = VI64 signed : rest })
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Store MemArg { offset }) =
            makeStoreInstr ctx { stack = rest } offset 4 v
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (I64Store MemArg { offset }) =
            makeStoreInstr ctx { stack = rest } offset 8 v
        step ctx@EvalCtx{ stack = (VF32 f:rest) } (F32Store MemArg { offset }) =
            makeStoreInstr ctx { stack = rest } offset 4 $ floatToWord f
        step ctx@EvalCtx{ stack = (VF64 f:rest) } (F64Store MemArg { offset }) =
            makeStoreInstr ctx { stack = rest } offset 8 $ doubleToWord f
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Store8 MemArg { offset }) =
            makeStoreInstr @Word8 ctx { stack = rest } offset 1 $ fromIntegral v
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (I32Store16 MemArg { offset }) =
            makeStoreInstr @Word16 ctx { stack = rest } offset 2 $ fromIntegral v
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (I64Store8 MemArg { offset }) =
            makeStoreInstr @Word8 ctx { stack = rest } offset 1 $ fromIntegral v
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (I64Store16 MemArg { offset }) =
            makeStoreInstr @Word16 ctx { stack = rest } offset 2 $ fromIntegral v
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (I64Store32 MemArg { offset }) =
            makeStoreInstr @Word32 ctx { stack = rest } offset 4 $ fromIntegral v
        step ctx@EvalCtx{ stack = st } MemorySize = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            size <- ((`quot` pageSize) . fromIntegral) <$> ByteArray.getSizeofMutableByteArray memory
            return $ Done ctx { stack = VI32 (fromIntegral size) : st }
        step ctx@EvalCtx{ stack = (VI32 n:rest) } MemoryGrow = do
            let MemoryInstance { lim = limit@(Limit _ maxLen), memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            size <- (`quot` pageSize) <$> ByteArray.getSizeofMutableByteArray memory
            let growTo = size + fromIntegral n
            let w64PageSize = fromIntegral $ pageSize `div` 8
            result <- (
                    if fromMaybe True ((growTo <=) . fromIntegral <$> maxLen) && growTo <= 0xFFFF
                    then (
                        if n == 0 then return size else do
                            mem' <- ByteArray.resizeMutableByteArray memory $ growTo * pageSize
                            ByteArray.setByteArray @Word64 mem' (size * w64PageSize) (fromIntegral n * w64PageSize) 0
                            writeIORef memoryRef mem'
                            return size
                    )
                    else return $ -1
                )
            return $ Done ctx { stack = VI32 (asWord32 $ fromIntegral result) : rest }
        step ctx@EvalCtx{ stack = (VI32 n:VI32 v:VI32 d:rest) } MemoryFill = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            size <- ByteArray.getSizeofMutableByteArray memory
            let dest = fromIntegral d
            let len = fromIntegral n
            if dest + len > size
            then return Trap
            else do
                ByteArray.setByteArray @Word8 memory dest len $ fromIntegral v
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 n:VI32 s:VI32 d:rest) } MemoryCopy = do
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            size <- ByteArray.getSizeofMutableByteArray memory
            let src = fromIntegral s
            let dest = fromIntegral d
            let len = fromIntegral n
            if dest + len > size || src + len > size
            then return Trap
            else do
                ByteArray.copyMutableByteArray memory dest memory src len
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 n:VI32 s:VI32 d:rest) } (MemoryInit dataIdx) = do
            let DataInstance {bytes, isDropped} = dataInstances store ! (dataaddrs moduleInstance ! fromIntegral dataIdx)
            let MemoryInstance { memory = memoryRef } = memInstances store ! (memaddrs moduleInstance ! 0)
            memory <- readIORef memoryRef
            size <- fromIntegral <$> ByteArray.getSizeofMutableByteArray memory
            let src = fromIntegral s
            let dest = fromIntegral d
            let len = fromIntegral n
            dropped <- readIORef isDropped
            if (dropped && len > 0) || src + len > LBS.length bytes || dest + len > size
            then return Trap
            else do
                mapM_ (uncurry $ ByteArray.writeByteArray memory) $ zip [fromIntegral d..] $
                    LBS.unpack $ LBS.take len $ LBS.drop src bytes
                return $ Done ctx { stack = rest }
        step ctx (DataDrop dataIdx) = do
            let DataInstance {isDropped} = dataInstances store ! (dataaddrs moduleInstance ! fromIntegral dataIdx)
            writeIORef isDropped True
            return $ Done ctx
        step ctx@EvalCtx{ stack = (VI32 n:VI32 s:VI32 d:rest) } (TableInit tableIdx elemIdx) = do
            let tableAddr = tableaddrs moduleInstance ! fromIntegral tableIdx
            let TableInstance { items } = tableInstances store ! tableAddr
            let elemAddr = elemaddrs moduleInstance ! fromIntegral elemIdx
            let ElemInstance {
                    eiItems = refs,
                    eiMode = mode,
                    isDropped = dropFlag
                } = elemInstances store ! elemAddr
            let src = fromIntegral s
            let dst = fromIntegral d
            let len = fromIntegral n
            els <- readIORef items
            isDropped <- readIORef dropFlag
            if src + len > Vector.length refs
                || dst + len > MVector.length els
                || (isDropped && len > 0)
                || isDeclarative mode
            then return Trap
            else do
                Vector.iforM_ (Vector.slice src len refs) $ \idx (RF fn) -> do
                    MVector.unsafeWrite els (dst + idx) (fromIntegral <$> fn)
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 n:VI32 s:VI32 d:rest) } (TableCopy toIdx fromIdx) = do
            let fromAddr = tableaddrs moduleInstance ! fromIntegral fromIdx
            let TableInstance { items = fromItems } = tableInstances store ! fromAddr
            let toAddr = tableaddrs moduleInstance ! fromIntegral toIdx
            let TableInstance { items = toItems } = tableInstances store ! toAddr
            let src = fromIntegral s
            let dst = fromIntegral d
            let len = fromIntegral n
            fromEls <- readIORef fromItems
            toEls <- readIORef toItems
            if src + len > MVector.length fromEls || dst + len > MVector.length toEls
            then return Trap
            else do
                let range = if dst <= src then [0..len - 1] else reverse [0..len - 1]
                flip mapM_ range $ \off -> do
                    el <- MVector.unsafeRead fromEls (src + off)
                    MVector.unsafeWrite toEls (dst + off) el
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 n:ref:VI32 i:rest) } (TableFill tableIdx) = do
            let tableAddr = tableaddrs moduleInstance ! fromIntegral tableIdx
            let TableInstance { items, t } = tableInstances store ! tableAddr
            let TableType (Limit _ max) _ = t
            let inc = fromIntegral n
            let from = fromIntegral i
            let val = case ref of
                    RE extRef -> fromIntegral <$> extRef
                    RF fnRef -> fromIntegral <$> fnRef
                    v -> error "Impossible due to validation"
            els <- readIORef items
            if from + inc > MVector.length els
            then return Trap
            else do
                Monad.forM_ [0..inc - 1] $ \off ->
                    MVector.unsafeWrite els (from + off) val
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack } (TableSize tableIdx) = do
            let tableAddr = tableaddrs moduleInstance ! fromIntegral tableIdx
            let TableInstance { items } = tableInstances store ! tableAddr
            len <- MVector.length <$> readIORef items
            return $ Done ctx { stack = VI32 (fromIntegral len) : stack }
        step ctx@EvalCtx{ stack = (VI32 growBy:ref:rest) } (TableGrow tableIdx) = do
            let tableAddr = tableaddrs moduleInstance ! fromIntegral tableIdx
            let TableInstance { items, t } = tableInstances store ! tableAddr
            let TableType (Limit _ max) _ = t
            let inc = fromIntegral growBy
            let val = case ref of
                    RE extRef -> extRef
                    RF fnRef -> fnRef
                    v -> error "Impossible due to validation"
            els <- readIORef items
            let currLen = MVector.length els
            let newLen = currLen + inc
            if maybe False ((newLen >) . fromIntegral) max || newLen > 0xFFFFFFFF
            then return $ Done ctx { stack = VI32 (asWord32 $ -1):rest }
            else do
                newEls <- MVector.grow els inc
                writeIORef items newEls
                Monad.forM_ [0..inc - 1] $ \off ->
                    MVector.unsafeWrite newEls (currLen + off) (fromIntegral <$> val)
                return $ Done ctx { stack = VI32 (fromIntegral currLen):rest }
        step ctx@EvalCtx{ stack = (ref:VI32 offset:rest) } (TableSet tableIdx) = do
            let tableAddr = tableaddrs moduleInstance ! fromIntegral tableIdx
            let TableInstance { items } = tableInstances store ! tableAddr
            let dst = fromIntegral offset
            let val = case ref of
                    RE extRef -> fromIntegral <$> extRef
                    RF fnRef -> fromIntegral <$> fnRef
                    v -> error "Impossible due to validation"
            els <- readIORef items
            if dst >= MVector.length els
            then return Trap
            else do
                MVector.unsafeWrite els dst val
                return $ Done ctx { stack = rest }
        step ctx@EvalCtx{ stack = (VI32 offset:rest) } (TableGet tableIdx) = do
            let tableAddr = tableaddrs moduleInstance ! fromIntegral tableIdx
            let TableInstance { t = TableType _ et, items } = tableInstances store ! tableAddr
            let dst = fromIntegral offset
            els <- readIORef items
            if dst >= MVector.length els
            then return Trap
            else do
                v <- MVector.unsafeRead els dst
                let val = (case et of {FuncRef -> RF; ExternRef -> RE}) (fromIntegral <$> v)
                return $ Done ctx { stack = val : rest }
        step ctx (ElemDrop elemIdx) = do
            let elemAddr = elemaddrs moduleInstance ! fromIntegral elemIdx
            let ElemInstance {isDropped} = elemInstances store ! elemAddr
            writeIORef isDropped True
            return $ Done ctx
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
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (IUnOp BS32 IExtend8S) =
            let byte = v .&. 0xFF in
            let r = if byte >= 0x80 then asWord32 (fromIntegral byte - 0x100) else byte in
            return $ Done ctx { stack = VI32 r : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (IUnOp BS32 IExtend16S) =
            let half = v .&. 0xFFFF in
            let r = if half >= 0x8000 then asWord32 (fromIntegral half - 0x10000) else half in
            return $ Done ctx { stack = VI32 r : rest }
        step ctx@EvalCtx{ stack = (VI32 v:rest) } (IUnOp BS32 IExtend32S) =
            return $ Done ctx { stack = VI32 v : rest }
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
            return $ Done ctx { stack = VI64 (v1 `shiftL` (fromIntegral (v2 `rem` 64))) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IShrU) =
            return $ Done ctx { stack = VI64 (v1 `shiftR` (fromIntegral (v2 `rem` 64))) : rest }
        step ctx@EvalCtx{ stack = (VI64 v2:VI64 v1:rest) } (IBinOp BS64 IShrS) =
            return $ Done ctx { stack = VI64 (asWord64 $ asInt64 v1 `shiftR` (fromIntegral (v2 `rem` 64))) : rest }
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
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (IUnOp BS64 IExtend8S) =
            let byte = v .&. 0xFF in
            let r = if byte >= 0x80 then asWord64 (fromIntegral byte - 0x100) else byte in
            return $ Done ctx { stack = VI64 r : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (IUnOp BS64 IExtend16S) =
            let quart = v .&. 0xFFFF in
            let r = if quart >= 0x8000 then asWord64 (fromIntegral quart - 0x10000) else quart in
            return $ Done ctx { stack = VI64 r : rest }
        step ctx@EvalCtx{ stack = (VI64 v:rest) } (IUnOp BS64 IExtend32S) =
            let half = v .&. 0xFFFFFFFF in
            let r = if half >= 0x80000000 then asWord64 (fromIntegral half - 0x100000000) else half in
            return $ Done ctx { stack = VI64 r : rest }
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
            if isNaN v || isInfinite v || v >= 2^32 || v <= -1
            then return Trap
            else return $ Done ctx { stack = VI32 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFU BS32 BS64) =
            if isNaN v || isInfinite v || v >= 2^32 || v <= -1
            then return Trap
            else return $ Done ctx { stack = VI32 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFU BS64 BS32) =
            if isNaN v || isInfinite v || v >= 2^64 || v <= -1
            then return Trap
            else return $ Done ctx { stack = VI64 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFU BS64 BS64) =
            if isNaN v || isInfinite v || v >= 2^64 || v <= -1
            then return Trap
            else return $ Done ctx { stack = VI64 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFS BS32 BS32) =
            if isNaN v || isInfinite v || v >= 2^31 || v < -2^31 - 1
            then return Trap
            else return $ Done ctx { stack = VI32 (asWord32 $ truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFS BS32 BS64) =
            if isNaN v || isInfinite v || v >= 2^31 || v <= -2^31 - 1
            then return Trap
            else return $ Done ctx { stack = VI32 (asWord32 $ truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncFS BS64 BS32) =
            if isNaN v || isInfinite v || v >= 2^63 || v < -2^63 - 1
            then return Trap
            else return $ Done ctx { stack = VI64 (asWord64 $ truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncFS BS64 BS64) =
            if isNaN v || isInfinite v || v >= 2^63 || v < -2^63 - 1
            then return Trap
            else return $ Done ctx { stack = VI64 (asWord64 $ truncate v) : rest }

        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFS BS32 BS32) | isNaN v =
            return $ Done ctx { stack = VI32 0 : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFS BS32 BS64) | isNaN v =
            return $ Done ctx { stack = VI32 0 : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFS BS64 BS32) | isNaN v =
            return $ Done ctx { stack = VI64 0 : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFS BS64 BS64) | isNaN v =
            return $ Done ctx { stack = VI64 0 : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFU BS32 BS32) | v <= -1 || isNaN v =
            return $ Done ctx { stack = VI32 0 : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFU BS32 BS64) | v <= -1 || isNaN v =
            return $ Done ctx { stack = VI32 0 : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFU BS64 BS32) | v <= -1 || isNaN v =
            return $ Done ctx { stack = VI64 0 : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFU BS64 BS64) | v <= -1 || isNaN v =
            return $ Done ctx { stack = VI64 0 : rest }

        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFS BS32 BS32) | v >= 2^31 =
            return $ Done ctx { stack = VI32 0x7fffffff : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFS BS32 BS64) | v >= 2^31 =
            return $ Done ctx { stack = VI32 0x7fffffff : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFS BS64 BS32) | v >= 2^63 =
            return $ Done ctx { stack = VI64 0x7fffffffffffffff : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFS BS64 BS64) | v >= 2^63 =
            return $ Done ctx { stack = VI64 0x7fffffffffffffff : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFU BS32 BS32) | v >= 2^32 =
            return $ Done ctx { stack = VI32 0xffffffff : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFU BS32 BS64) | v >= 2^32 =
            return $ Done ctx { stack = VI32 0xffffffff : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFU BS64 BS32) | v >= 2^64 =
            return $ Done ctx { stack = VI64 0xffffffffffffffff : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFU BS64 BS64) | v >= 2^64 =
            return $ Done ctx { stack = VI64 0xffffffffffffffff : rest }
        
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFS BS32 BS32) | v <= -2^31 - 1 =
            return $ Done ctx { stack = VI32 0x80000000 : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFS BS32 BS64) | v <= -2^31 - 1 =
            return $ Done ctx { stack = VI32 0x80000000 : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFS BS64 BS32) | v <= -2^63 - 1 =
            return $ Done ctx { stack = VI64 0x8000000000000000 : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFS BS64 BS64) | v <= -2^63 - 1 =
            return $ Done ctx { stack = VI64 0x8000000000000000 : rest }

        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFU BS32 BS32) =
            return $ Done ctx { stack = VI32 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFU BS32 BS64) =
            return $ Done ctx { stack = VI32 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFU BS64 BS32) =
            return $ Done ctx { stack = VI64 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFU BS64 BS64) =
            return $ Done ctx { stack = VI64 (truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFS BS32 BS32) =
            return $ Done ctx { stack = VI32 (asWord32 $ truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFS BS32 BS64) =
            return $ Done ctx { stack = VI32 (asWord32 $ truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF32 v:rest) } (ITruncSatFS BS64 BS32) =
            return $ Done ctx { stack = VI64 (asWord64 $ truncate v) : rest }
        step ctx@EvalCtx{ stack = (VF64 v:rest) } (ITruncSatFS BS64 BS64) =
            return $ Done ctx { stack = VI64 (asWord64 $ truncate v) : rest }
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
eval _ _ _ HostInstance { funcType, hostCode } args = Just <$> hostCode args

invoke :: Store -> ModuleInstance -> Address -> [Value] -> IO (Maybe [Value])
invoke st inst funcIdx = eval defaultBudget st inst $ funcInstances st ! funcIdx

invokeExport :: Store -> ModuleInstance -> TL.Text -> [Value] -> IO (Maybe [Value])
invokeExport st inst@ModuleInstance { exports } name args =
    case Vector.find (\(ExportInstance n _) -> n == name) exports of
        Just (ExportInstance _ (ExternFunction addr)) -> invoke st inst addr args
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

-- | Retrieve mutable memory from the 'Store'
getMemory :: Store -> Address -> Maybe MemoryInstance
getMemory Store{memInstances} address = memInstances !? address

