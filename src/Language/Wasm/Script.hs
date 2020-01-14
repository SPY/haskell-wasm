{-# LANGUAGE OverloadedStrings #-}
module Language.Wasm.Script (
    runScript,
    OnAssertFail
) where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding
import Numeric.IEEE (identicalIEEE)
import qualified Control.DeepSeq as DeepSeq
import Data.Maybe (fromJust, isNothing)

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

runScript :: OnAssertFail -> Script -> IO ()
runScript onAssertFail script = do
    (globI32, globF32, globF64) <- hostGlobals
    (st, inst) <- Interpreter.makeHostModule Interpreter.emptyStore [
            ("print", hostPrint []),
            ("print_i32", hostPrint [Struct.I32]),
            ("print_i32_f32", hostPrint [Struct.I32, Struct.F32]),
            ("print_f64_f64", hostPrint [Struct.F64, Struct.F64]),
            ("print_f32", hostPrint [Struct.F32]),
            ("print_f64", hostPrint [Struct.F64]),
            ("global_i32", globI32),
            ("global_f32", globF32),
            ("global_f64", globF64),
            ("memory", Interpreter.HostMemory $ Struct.Limit 1 (Just 2)),
            ("table", Interpreter.HostTable $ Struct.Limit 10 (Just 20))
        ]
    go script $ emptyState { store = st, moduleRegistery = Map.singleton "spectest" inst }
    where
        hostPrint paramTypes = Interpreter.HostFunction (Struct.FuncType paramTypes []) (\args -> return [])
        hostGlobals = do
            globI32 <- Interpreter.makeMutGlobal $ Interpreter.VI32 666
            globF32 <- Interpreter.makeMutGlobal $ Interpreter.VF32 666
            globF64 <- Interpreter.makeMutGlobal $ Interpreter.VF64 666
            return (Interpreter.HostGlobal globI32, Interpreter.HostGlobal globF32, Interpreter.HostGlobal globF64)

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
                    res <- Interpreter.instantiate (store st) (buildImports st) m
                    case res of
                        Right (modInst, store') -> return $ addToStore ident modInst $ st { lastModule = Just modInst, store = store' }
                        Left reason -> error $ "Module instantiation failed dut to invalid module with reason: " ++ show reason
                Left reason -> error $ "Module instantiation failed dut to invalid module with reason: " ++ show reason
        
        getModule :: ScriptState -> Maybe Ident -> Maybe Interpreter.ModuleInstance
        getModule st (Just (Ident i)) = Map.lookup i (modules st)
        getModule st Nothing = lastModule st

        asArg :: Struct.Expression -> Interpreter.Value
        asArg [Struct.I32Const v] = Interpreter.VI32 v
        asArg [Struct.F32Const v] = Interpreter.VF32 v
        asArg [Struct.I64Const v] = Interpreter.VI64 v
        asArg [Struct.F64Const v] = Interpreter.VF64 v
        asArg _                   = error "Only const instructions supported as arguments for actions"

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
        isValueEqual (Interpreter.VF32 v1) (Interpreter.VF32 v2) = identicalIEEE v1 v2
        isValueEqual (Interpreter.VF64 v1) (Interpreter.VF64 v2) = identicalIEEE v1 v2
        isValueEqual _ _ = False

        isNaNReturned :: ScriptState -> Action -> Assertion -> IO ()
        isNaNReturned st action assert = do
            result <- runAction st action
            case result of
                Just [Interpreter.VF32 v] ->
                    if isNaN v
                    then return ()
                    else onAssertFail ("Expected NaN, but action returned " ++ show v) assert
                Just [Interpreter.VF64 v] ->
                    if isNaN v
                    then return ()
                    else onAssertFail ("Expected NaN, but action returned " ++ show v) assert
                _ -> onAssertFail ("Expected NaN, but action returned " ++ show result) assert
        
        buildModule :: ModuleDef -> (Maybe Ident, Struct.Module)
        buildModule (RawModDef ident m) = (ident, m)
        buildModule (TextModDef ident textRep) =
            let Right m = Lexer.scanner (TLEncoding.encodeUtf8 textRep) >>= Parser.parseModule in
            (ident, m)
        buildModule (BinaryModDef ident binaryRep) =
            let Right m = Binary.decodeModuleLazy binaryRep in
            (ident, m)

        checkModuleInvalid :: Struct.Module -> IO ()
        checkModuleInvalid _ = return ()

        getFailureString :: Validate.ValidationError -> [TL.Text]
        getFailureString (Validate.TypeMismatch _ _) = ["type mismatch"]
        getFailureString Validate.ResultTypeDoesntMatch = ["type mismatch"]
        getFailureString Validate.MoreThanOneMemory = ["multiple memories"]
        getFailureString Validate.MoreThanOneTable = ["multiple tables"]
        getFailureString Validate.LocalIndexOutOfRange = ["unknown local"]
        getFailureString Validate.MemoryIndexOutOfRange = ["unknown memory", "unknown memory 0"]
        getFailureString Validate.TableIndexOutOfRange = ["unknown table", "unknown table 0"]
        getFailureString Validate.FunctionIndexOutOfRange = ["unknown function", "unknown function 0"]
        getFailureString Validate.GlobalIndexOutOfRange = ["unknown global"]
        getFailureString Validate.LabelIndexOutOfRange = ["unknown label"]
        getFailureString Validate.TypeIndexOutOfRange = ["unknown type"]
        getFailureString Validate.MinMoreThanMaxInMemoryLimit = ["size minimum must not be greater than maximum"]
        getFailureString Validate.MemoryLimitExceeded = ["memory size must be at most 65536 pages (4GiB)"]
        getFailureString Validate.AlignmentOverflow = ["alignment", "alignment must not be larger than natural"]
        getFailureString (Validate.DuplicatedExportNames _) = ["duplicate export name"]
        getFailureString Validate.InvalidConstantExpr = ["constant expression required"]
        getFailureString Validate.InvalidResultArity = ["invalid result arity"]
        getFailureString Validate.GlobalIsImmutable = ["global is immutable"]
        getFailureString Validate.ImportedGlobalIsNotConst = ["mutable globals cannot be imported"]
        getFailureString Validate.ExportedGlobalIsNotConst = ["mutable globals cannot be exported"]
        getFailureString Validate.InvalidStartFunctionType = ["start function"]
        getFailureString r = [TL.concat ["not implemented ", (TL.pack $ show r)]]

        runAssert :: ScriptState -> Assertion -> IO ()
        runAssert st assert@(AssertReturn action expected) = do
            result <- runAction st action
            case result of
                Just result -> do
                    if length result == length expected && (all id $ zipWith isValueEqual result (map asArg expected))
                    then return ()
                    else onAssertFail ("Expected " ++ show (map asArg expected) ++ ", but action returned " ++ show result) assert
                Nothing -> onAssertFail ("Expected " ++ show (map asArg expected) ++ ", but action returned Trap") assert
        runAssert st assert@(AssertReturnCanonicalNaN action) = isNaNReturned st action assert
        runAssert st assert@(AssertReturnArithmeticNaN action) = isNaNReturned st action assert
        runAssert st assert@(AssertInvalid moduleDef failureString) =
            let (_, m) = buildModule moduleDef in
            case Validate.validate m of
                Right _ -> onAssertFail "An invalid module passed validation step" assert
                Left reason ->
                    if failureString `elem` getFailureString reason
                    then return ()
                    else
                        let msg = "Module is invalid for other reason. Expected "
                                ++ show failureString
                                ++ ", but actual is "
                                ++ show (getFailureString reason)
                        in onAssertFail msg assert
        runAssert st assert@(AssertMalformed (TextModDef _ textRep) failureString) =
            case DeepSeq.force $ Lexer.scanner (TLEncoding.encodeUtf8 textRep) >>= Parser.parseModule of
                Right _ -> onAssertFail ("Module parsing should fail with failure string " ++ show failureString) assert
                Left _ -> return ()
        runAssert st assert@(AssertMalformed (BinaryModDef ident binaryRep) failureString) =
            case Binary.decodeModuleLazy binaryRep of
                Right _ -> onAssertFail ("Module decoding should fail with failure string " ++ show failureString) assert
                Left _ -> return ()
        runAssert st assert@(AssertMalformed (RawModDef _ _) failureString) = return ()
        runAssert st assert@(AssertUnlinkable moduleDef failureString) =
            let (_, m) = buildModule moduleDef in
            case Validate.validate m of
                Right m -> do
                    res <- Interpreter.instantiate (store st) (buildImports st) m
                    case res of
                        Left err -> return ()
                        Right _ -> onAssertFail ("Module linking should fail with failure string " ++ show failureString) assert
                Left reason -> error $ "Module linking failed dut to invalid module with reason: " ++ show reason
        runAssert st assert@(AssertTrap (Left action) failureString) = do
            result <- runAction st action
            if isNothing result
            then return ()
            else onAssertFail ("Expected trap, but action returned " ++ show (fromJust result)) assert
        runAssert st assert@(AssertTrap (Right moduleDef) failureString) =
            let (_, m) = buildModule moduleDef in
            case Validate.validate m of
                Right m -> do
                    res <- Interpreter.instantiate (store st) (buildImports st) m
                    case res of
                        Left "Start function terminated with trap" -> return ()
                        _ -> onAssertFail ("Module linking should fail with trap during execution of a start function") assert
                Left reason -> error $ "Module linking failed dut to invalid module with reason: " ++ show reason
        runAssert st assert@(AssertExhaustion action failureString) = do
            result <- runAction st action
            if isNothing result
            then return ()
            else onAssertFail ("Expected exhaustion, but action returned " ++ show (fromJust result)) assert

        runCommand :: ScriptState -> Command -> IO ScriptState
        runCommand st (ModuleDef moduleDef) =
            let (ident, m) = buildModule moduleDef in
            addModule ident m st
        runCommand st (Register name i) = return $ addToRegistery name i st
        runCommand st (Action action) = runAction st action >> return st
        runCommand st (Assertion assertion) = runAssert st assertion >> return st
        runCommand st _ = return st
