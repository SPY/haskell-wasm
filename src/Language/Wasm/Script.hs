module Language.Wasm.Script (
    runScript,
    OnAssertFail
) where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding

import Language.Wasm.Parser (
        Ident(..),
        Script,
        ModuleDef(..),
        Command(..),
        Action(..),
        Assertion(..),
        Meta(..)
    )

import qualified Language.Wasm.Interpreter as Interpreter
import qualified Language.Wasm.Validate as Validate
import qualified Language.Wasm.Structure as Struct
import qualified Language.Wasm.Parser as Parser
import qualified Language.Wasm.Lexer as Lexer
import qualified Language.Wasm.Binary as Binary

type OnAssertFail = Assertion -> IO ()

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
runScript onAssertFail script = go script emptyState
    where
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
                Validate.Valid -> do
                    (modInst, store') <- Interpreter.instantiate (store st) (buildImports st) m
                    return $ addToStore ident modInst $ st { lastModule = Just modInst, store = store' }
                reason -> error $ "Module instantiation failed dut to invalid module with reason: " ++ show reason
        
        getModule :: ScriptState -> Maybe Ident -> Maybe Interpreter.ModuleInstance
        getModule st (Just (Ident i)) = Map.lookup i (modules st)
        getModule st Nothing = lastModule st

        runCommand :: ScriptState -> Command -> IO ScriptState
        runCommand st (ModuleDef (RawModDef ident m)) = addModule ident m st
        runCommand st (ModuleDef (TextModDef ident textRep)) =
            let Right m = Parser.parseModule <$> Lexer.scanner (TLEncoding.encodeUtf8 textRep) in
            addModule ident m st
        runCommand st (ModuleDef (BinaryModDef ident binaryRep)) =
            let Right m = Binary.decodeModuleLazy binaryRep in
            addModule ident m st
        runCommand st (Register name i) = return $ addToRegistery name i st
        runCommand st _ = return st
