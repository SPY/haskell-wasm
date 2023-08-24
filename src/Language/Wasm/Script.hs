{-# LANGUAGE OverloadedStrings #-}
module Language.Wasm.Script (
    runScript,
    OnAssertFail
) where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding
import qualified Control.Monad.State as State
import Control.Monad.IO.Class (liftIO)
import Numeric.IEEE (identicalIEEE)
import qualified Control.DeepSeq as DeepSeq
import Data.Maybe (fromJust, isNothing)
import Debug.Trace (trace)

import Language.Wasm.Parser (
        Ident(..),
        Script,
        ModuleDef(..),
        Command(..),
        Action(..),
        Assertion(..)
    )

import qualified Language.Wasm.Interpreter as Interpreter
import qualified Language.Wasm.Validate as Validate
import qualified Language.Wasm.Structure as Struct
import qualified Language.Wasm.Parser as Parser
import qualified Language.Wasm.Lexer as Lexer
import qualified Language.Wasm.Binary as Binary

type OnAssertFail = String -> Assertion -> IO ()

data ScriptState = ScriptState {
    store :: Interpreter.Store,
    lastModule :: Maybe Interpreter.ModuleInstance,
    modules :: Map.Map TL.Text Interpreter.ModuleInstance,
    moduleRegistery :: Map.Map TL.Text Interpreter.ModuleInstance
}

emptyState :: ScriptState
emptyState = ScriptState {
    store = Interpreter.emptyStore,
    lastModule = Nothing,
    modules = Map.empty,
    moduleRegistery = Map.empty
}

type AssertM = State.StateT (ScriptState, String) IO

runScript :: OnAssertFail -> Script -> IO ()
runScript onAssertFail script = do
    (globI32, globI64, globF32, globF64) <- hostGlobals
    (st, inst) <- Interpreter.makeHostModule Interpreter.emptyStore [
            ("print", hostPrint []),
            ("print_i32", hostPrint [Struct.I32]),
            ("print_i64", hostPrint [Struct.I64]),
            ("print_i32_f32", hostPrint [Struct.I32, Struct.F32]),
            ("print_f64_f64", hostPrint [Struct.F64, Struct.F64]),
            ("print_f32", hostPrint [Struct.F32]),
            ("print_f64", hostPrint [Struct.F64]),
            ("global_i32", globI32),
            ("global_i64", globI64),
            ("global_f32", globF32),
            ("global_f64", globF64),
            ("memory", Interpreter.HostMemory $ Struct.Limit 1 (Just 2)),
            ("table", Interpreter.HostTable $ Struct.Limit 10 (Just 20))
        ]
    go script $ emptyState { store = st, moduleRegistery = Map.singleton "spectest" inst }
    where
        hostPrint paramTypes = Interpreter.HostFunction (Struct.FuncType paramTypes []) (\args -> return [])
        hostGlobals = do
            let globI32 = Interpreter.makeConstGlobal $ Interpreter.VI32 666
            let globI64 = Interpreter.makeConstGlobal $ Interpreter.VI64 666
            let globF32 = Interpreter.makeConstGlobal $ Interpreter.VF32 666
            let globF64 = Interpreter.makeConstGlobal $ Interpreter.VF64 666
            return (
                    Interpreter.HostGlobal globI32,
                    Interpreter.HostGlobal globI64,
                    Interpreter.HostGlobal globF32,
                    Interpreter.HostGlobal globF64
                )

        go [] _ = return ()
        go (c:cs) st = runCommand st c >>= go cs
        
        addToRegistery :: TL.Text -> Maybe Ident -> ScriptState -> ScriptState
        addToRegistery name i st =
            case getModule st i of
                Just m -> st { moduleRegistery = Map.insert name m $ moduleRegistery st }
                Nothing -> error $ "Cannot register module with identifier '" ++ show i  ++ "'. No such module"

        addToStore :: Maybe Ident -> Interpreter.ModuleInstance -> ScriptState -> ScriptState
        addToStore (Just (Ident ident)) m st = st { modules = Map.insert ident m $ modules st }
        addToStore Nothing _ st = st

        buildImports :: ScriptState -> Interpreter.Imports
        buildImports st =
            Map.fromList $ concat $ map toImports $ Map.toList $ moduleRegistery st
            where
                toImports :: (TL.Text, Interpreter.ModuleInstance) -> [((TL.Text, TL.Text), Interpreter.ExternalValue)]
                toImports (modName, mod) = map (asImport modName) $ Vector.toList $ Interpreter.exports mod
                asImport :: TL.Text -> Interpreter.ExportInstance -> ((TL.Text, TL.Text), Interpreter.ExternalValue)
                asImport modName (Interpreter.ExportInstance name val) = ((modName, name), val)

        addModule :: Maybe Ident -> Struct.Module -> ScriptState -> IO ScriptState
        addModule ident m st =
            case Validate.validate m of
                Right m -> do
                    (res, store') <- Interpreter.instantiate (store st) (buildImports st) m
                    case res of
                        Right modInst -> return $ addToStore ident modInst $ st { lastModule = Just modInst, store = store' }
                        Left reason -> error $ "Module instantiation failed due to invalid module with reason: " ++ show reason
                Left reason -> error $ "Module instantiation failed due to invalid module with reason: " ++ show reason
        
        getModule :: ScriptState -> Maybe Ident -> Maybe Interpreter.ModuleInstance
        getModule st (Just (Ident i)) = Map.lookup i (modules st)
        getModule st Nothing = lastModule st

        asArg :: Struct.Expression -> Interpreter.Value
        asArg [Struct.I32Const v] = Interpreter.VI32 v
        asArg [Struct.F32Const v] = Interpreter.VF32 v
        asArg [Struct.I64Const v] = Interpreter.VI64 v
        asArg [Struct.F64Const v] = Interpreter.VF64 v
        asArg [Struct.RefNull Struct.FuncRef] = Interpreter.RF Nothing
        asArg [Struct.RefNull Struct.ExternRef] = Interpreter.RE Nothing
        asArg [Struct.RefExtern v] = Interpreter.RE (Just v)
        asArg expr = error $ "Only const instructions supported as arguments for actions: " ++ show expr

        runAction :: ScriptState -> Action -> IO (Maybe [Interpreter.Value])
        runAction st (Invoke ident name args) = do
            case getModule st ident of
                Just m -> Interpreter.invokeExport (store st) m name $ map asArg args
                Nothing -> error $ "Cannot invoke function on module with identifier '" ++ show ident  ++ "'. No such module"
        runAction st (Get ident name) = do
            case getModule st ident of
                Just m -> Interpreter.getGlobalValueByName (store st) m name >>= return . Just . (: [])
                Nothing -> error $ "Cannot invoke function on module with identifier '" ++ show ident  ++ "'. No such module"

        isValueEqual :: Interpreter.Value -> Interpreter.Value -> Bool
        isValueEqual (Interpreter.VI32 v1) (Interpreter.VI32 v2) = v1 == v2
        isValueEqual (Interpreter.VI64 v1) (Interpreter.VI64 v2) = v1 == v2
        isValueEqual (Interpreter.VF32 v1) (Interpreter.VF32 v2) = (isNaN v1 && isNaN v2) || identicalIEEE v1 v2
        isValueEqual (Interpreter.VF64 v1) (Interpreter.VF64 v2) = (isNaN v1 && isNaN v2) || identicalIEEE v1 v2
        isValueEqual (Interpreter.RF f1) (Interpreter.RF f2) = f1 == f2
        isValueEqual (Interpreter.RE e1) (Interpreter.RE e2) = e1 == e2
        isValueEqual _ _ = False

        isNaNReturned :: Action -> Assertion -> AssertM ()
        isNaNReturned action assert = do
            result <- runActionInAssert action
            case result of
                Just [Interpreter.VF32 v] ->
                    if isNaN v
                    then return ()
                    else printFailedAssert ("Expected NaN, but action returned " ++ show v) assert
                Just [Interpreter.VF64 v] ->
                    if isNaN v
                    then return ()
                    else printFailedAssert ("Expected NaN, but action returned " ++ show v) assert
                _ -> printFailedAssert ("Expected NaN, but action returned " ++ show result) assert
        
        buildModule :: ModuleDef -> (Maybe Ident, Struct.Module)
        buildModule (RawModDef ident m) = (ident, m)
        buildModule (TextModDef ident textRep) =
            let Right m = Lexer.scanner (TLEncoding.encodeUtf8 textRep) >>= Parser.parseModule in
            (ident, m)
        buildModule (BinaryModDef ident binaryRep) =
            let Right m = Binary.decodeModuleLazy binaryRep  in
            (ident, m)

        checkModuleInvalid :: Struct.Module -> IO ()
        checkModuleInvalid _ = return ()

        getFailureString :: Validate.ValidationError -> [TL.Text]
        getFailureString (Validate.TypeMismatch _ _) = ["type mismatch"]
        getFailureString (Validate.RefTypeMismatch _ _) = ["type mismatch"]
        getFailureString Validate.ResultTypeDoesntMatch = ["type mismatch"]
        getFailureString Validate.MoreThanOneMemory = ["multiple memories"]
        getFailureString (Validate.LocalIndexOutOfRange idx) = ["unknown local", "unknown local " <> TL.pack (show idx)]
        getFailureString (Validate.MemoryIndexOutOfRange idx) = ["unknown memory", "unknown memory " <> TL.pack (show idx)]
        getFailureString (Validate.TableIndexOutOfRange idx) = ["unknown table", "unknown table " <> TL.pack (show idx)]
        getFailureString (Validate.FunctionIndexOutOfRange idx) = ["unknown function", "unknown function " <> TL.pack (show idx)]
        getFailureString (Validate.GlobalIndexOutOfRange idx) = ["unknown global", "unknown global " <> TL.pack (show idx)]
        getFailureString Validate.LabelIndexOutOfRange = ["unknown label"]
        getFailureString Validate.TypeIndexOutOfRange = ["unknown type"]
        getFailureString Validate.MinMoreThanMaxInMemoryLimit = ["size minimum must not be greater than maximum"]
        getFailureString Validate.MemoryLimitExceeded = ["memory size must be at most 65536 pages (4GiB)"]
        getFailureString Validate.AlignmentOverflow = ["alignment", "alignment must not be larger than natural"]
        getFailureString (Validate.DuplicatedExportNames _) = ["duplicate export name"]
        getFailureString Validate.InvalidConstantExpr = ["constant expression required"]
        getFailureString Validate.InvalidResultArity = ["invalid result arity"]
        getFailureString Validate.GlobalIsImmutable = ["global is immutable"]
        getFailureString Validate.InvalidStartFunctionType = ["start function"]
        getFailureString Validate.InvalidTableType = ["size minimum must not be greater than maximum"]
        getFailureString (Validate.ElemIndexOutOfRange idx) = ["unknown elem segment " <> TL.pack (show idx)]
        getFailureString (Validate.DataIndexOutOfRange idx) = ["unknown data segment", "unknown data segment " <> TL.pack (show idx)]
        getFailureString (Validate.UndeclaredFunctionRef _) = ["undeclared function reference"]
        getFailureString r = [TL.concat ["not implemented ", TL.pack $ show r]]

        printFailedAssert :: String -> Assertion -> AssertM ()
        printFailedAssert msg assert = do
            (_, pos) <- State.get
            liftIO $ onAssertFail (pos ++ ": " ++ msg) assert

        runActionInAssert :: Action -> AssertM (Maybe [Interpreter.Value])
        runActionInAssert action = do
            (st, _) <- State.get
            liftIO $ runAction st action

        runAssert :: Assertion -> AssertM ()
        runAssert assert@(AssertReturn action expected) = do
            (st, _) <- State.get
            result <- runActionInAssert action
            case result of
                Just result -> do
                    if length result == length expected && (all id $ zipWith isValueEqual result (map asArg expected))
                    then return ()
                    else printFailedAssert ("Expected " ++ show (map asArg expected) ++ ", but action returned " ++ show result) assert
                Nothing -> printFailedAssert ("Expected " ++ show (map asArg expected) ++ ", but action returned Trap") assert
        runAssert assert@(AssertReturnCanonicalNaN action) = isNaNReturned action assert
        runAssert assert@(AssertReturnArithmeticNaN action) = isNaNReturned action assert
        runAssert assert@(AssertInvalid moduleDef failureString) =
            let (_, m) = buildModule moduleDef in
            case Validate.validate m of
                Right _ -> printFailedAssert "An invalid module passed validation step" assert
                Left reason ->
                    if failureString `elem` getFailureString reason
                    then return ()
                    else
                        let msg = "Module is invalid for other reason. Expected "
                                ++ show failureString
                                ++ ", but actual is "
                                ++ show (getFailureString reason)
                        in printFailedAssert msg assert
        runAssert assert@(AssertMalformed (TextModDef _ textRep) failureString) =
            case DeepSeq.force $ Lexer.scanner (TLEncoding.encodeUtf8 textRep) >>= Parser.parseModule of
                Right _ -> printFailedAssert ("Module parsing should fail with failure string " ++ show failureString) assert
                Left _ -> return ()
        runAssert assert@(AssertMalformed (BinaryModDef ident binaryRep) failureString) =
            case Binary.decodeModuleLazy binaryRep of
                Right _ -> printFailedAssert ("Module decoding should fail with failure string " ++ show failureString) assert
                Left _ -> return ()
        runAssert assert@(AssertMalformed (RawModDef _ _) failureString) = return ()
        runAssert assert@(AssertUnlinkable moduleDef failureString) =
            let (_, m) = buildModule moduleDef in
            case Validate.validate m of
                Right m -> do
                    st <- fst <$> State.get
                    (res, _) <- liftIO $ Interpreter.instantiate (store st) (buildImports st) m
                    case res of
                        Left err -> return ()
                        Right _ -> printFailedAssert ("Module linking should fail with failure string " ++ show failureString) assert
                Left reason -> error $ "Module linking failed due to invalid module with reason: " ++ show reason
        runAssert assert@(AssertTrap (Left action) failureString) = do
            result <- runActionInAssert action
            if isNothing result
            then return ()
            else printFailedAssert ("Expected trap, but action returned " ++ show (fromJust result)) assert
        runAssert assert@(AssertTrap (Right moduleDef) failureString) =
            let (_, m) = buildModule moduleDef in
            case Validate.validate m of
                Right m -> do
                    (st, pos) <- State.get
                    (res, store') <- liftIO $ Interpreter.instantiate (store st) (buildImports st) m
                    State.put (st { store = store' }, pos)
                    case res of
                        Left err | err == TL.unpack failureString -> return ()
                        Left "Start function terminated with trap" ->
                            State.modify $ \(st, pos) -> (st { store = store' }, pos)
                        r -> printFailedAssert "Module linking should fail with trap during execution of a start function" assert
                Left reason -> error $ "Module linking failed due to invalid module with reason: " ++ show reason
        runAssert assert@(AssertExhaustion action failureString) = do
            result <- runActionInAssert action
            if isNothing result
            then return ()
            else printFailedAssert ("Expected exhaustion, but action returned " ++ show (fromJust result)) assert

        runCommand :: ScriptState -> Command -> IO ScriptState
        runCommand st (ModuleDef moduleDef) =
            let (ident, m) = buildModule moduleDef in
            addModule ident m st
        runCommand st (Register name i) = return $ addToRegistery name i st
        runCommand st (Action action) = runAction st action >> return st
        runCommand st (Assertion pos assertion) = do
            fst <$> flip State.execStateT (st, ("Line " ++ show pos)) (runAssert assertion)
        runCommand st _ = return st
