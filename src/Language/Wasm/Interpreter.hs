{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Wasm.Interpreter (
    instantiate
) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as LBS

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.IORef (IORef)
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import Data.Word (Word32, Word64)
import Numeric.Natural (Natural)

import Language.Wasm.Structure as Struct

data Value =
    VI32 Word32
    | VI62 Word64
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
    maxLen :: Int
}

data MemoryInstance = MemoryInstance {
    memory :: IOArray Int Word32,
    maxLen :: Int -- in page size (64Ki)
}

data GlobalInstance = GIConst Value | GIMut (IORef Value)

data ExportInstance = ExportInstance TL.Text ExternalVal deriving (Eq, Show)

data ExternalVal =
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
    functions :: Vector FunctionInstance,
    tables :: Vector TableInstance,
    mems :: Vector MemoryInstance,
    globals :: Vector GlobalInstance
}

initialStore :: Store
initialStore = Store {
    functions = Vector.empty,
    tables = Vector.empty,
    mems = Vector.empty,
    globals = Vector.empty
}

data ModuleInstance = ModuleInstance {
    types :: Vector FuncType,
    functions :: Vector Address,
    tables :: Vector Address,
    mems :: Vector Address,
    globals :: Vector Address,
    exports :: Vector ExportInstance
} deriving (Eq, Show)

instantiate :: Store -> Module -> IO (ModuleInstance, Store)
instantiate st mod = do
    return $ (
            ModuleInstance {
                types = Vector.fromList $ Struct.types mod
            },
            st
        )
