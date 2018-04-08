module Language.Wasm.Script (
    runScript,
    OnAssertFail
) where

import qualified Data.Map as Map
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
        
        addToRegistery :: Maybe Ident -> Interpreter.ModuleInstance -> ScriptState -> ScriptState
        addToRegistery (Just (Ident ident)) m st = st { moduleRegistery = Map.insert ident m $ moduleRegistery st }
        addToRegistery Nothing _ st = st

        addModule :: Maybe Ident -> Struct.Module -> ScriptState -> IO ScriptState
        addModule ident m st =
            case Validate.validate m of
                Validate.Valid -> do
                    (modInst, store') <- Interpreter.instantiate (store st) Interpreter.emptyImports m
                    return $ addToRegistery ident modInst $ st { lastModule = Just modInst, store = store' }
                reason -> error $ "Module instantiation failed dut to invalid module with reason: " ++ show reason

        runCommand :: ScriptState -> Command -> IO ScriptState
        runCommand st (ModuleDef (RawModDef ident m)) = addModule ident m st
        runCommand st (ModuleDef (TextModDef ident textRep)) =
            let Right m = Parser.parseModule <$> Lexer.scanner (TLEncoding.encodeUtf8 textRep) in
            addModule ident m st
        runCommand st (ModuleDef (BinaryModDef ident binaryRep)) =
            let Right m = Binary.decodeModuleLazy binaryRep in
            addModule ident m st
        runCommand st _ = return st
