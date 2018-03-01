{-# LANGUAGE DuplicateRecordFields #-}

module Language.Wasm.Interpreter (
    instantiate
) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.IORef (IORef)
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import Data.Word (Word32, Word64)

import Language.Wasm.Structure

data Value =
    VI32 Word32
    | VI62 Word64
    | VF32 Float
    | VF64 Double

type Address = Int

data Ref a = Imported TL.Text TL.Text | Local a

type TableInstance = Vector (Maybe Address)

type MemoryInstance = IOArray Int Word32

data GlobalInstance = GConst Value | GMut (IORef Value)

data ModuleInstance = ModuleInstance {
    types :: Vector FuncType,
    functions :: Vector (Ref Function),
    tables :: Vector (Ref TableInstance),
    mems :: Vector (Ref MemoryInstance),
    globals :: Vector (Ref Global),
    exports :: Map.Map TL.Text ExportDesc
}

instantiate :: Module -> ModuleInstance
instantiate mod = undefined