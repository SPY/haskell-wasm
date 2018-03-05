{-# LANGUAGE DuplicateRecordFields #-}

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

import Language.Wasm.Structure

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

data Ref a = Imported TL.Text TL.Text | Local a

type TableInstance = Vector (Maybe Address)

type MemoryInstance = IOArray Int Word32

data GlobalInstance = GConst Value | GMut (IORef Value)

data FunctionInstance = FunctionInstance {
    funcType :: FuncType,
    moduleInstance :: ModuleInstance,
    code :: Function
} deriving (Show, Eq)

data Store = Store {
    functions :: Vector FunctionInstance,
    tables :: Vector TableInstance,
    mems :: Vector MemoryInstance,
    globals :: Vector Global
}

data ModuleInstance = ModuleInstance {
    types :: Vector FuncType,
    functions :: Vector Address,
    tables :: Vector Address,
    mems :: Vector Address,
    globals :: Vector Address,
    exports :: Map.Map TL.Text ExportDesc
} deriving (Eq, Show)

instantiate :: Store -> Module -> (ModuleInstance, Store)
instantiate mod = undefined