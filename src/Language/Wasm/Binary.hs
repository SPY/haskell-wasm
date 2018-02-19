{-# LANGUAGE NamedFieldPuns #-}

module Language.Wasm.Binary (
    
) where

import Language.Wasm.Structure

import Numeric.Natural (Natural)
import Data.Bits
import Data.Word (Word8)
import Data.Serialize
import qualified Data.ByteString as BS

putULEB128 :: Natural -> Put
putULEB128 val =
    if val < 128
    then putWord8 $ fromIntegral val
    else do
        putWord8 $ 0x80 + (0x7F .&. fromIntegral val)
        putULEB128 $ val `shiftR` 7

getULEB128 :: Get Natural
getULEB128 = do
    val <- getWord8
    if val < 2 ^ 7
    then return $ fromIntegral val
    else do
        rest <- getULEB128
        return $ (fromIntegral $ 0x7F .&. val) + 128 * rest

putVec :: Serialize a => [a] -> Put
putVec list = do
    putULEB128 $ fromIntegral $ length list
    mapM put list
    return ()

getVec :: Serialize a => Get [a]
getVec = do
    len <- getULEB128
    sequence $ replicate (fromIntegral len) get

byteGuard :: Word8 -> Get ()
byteGuard expected = do
    byte <- getWord8
    if byte == expected
    then return ()
    else fail $ "Expected " ++ show expected ++ ", but encountered " ++ show byte

putSection :: Serialize a => SectionType -> a -> Put
putSection section content = do
    put section
    let payload = encode content
    putULEB128 $ fromIntegral $ BS.length payload
    putByteString payload

data SectionType =
    CustomSection
    | TypeSection
    | ImportSection
    | FunctionSection
    | TableSection
    | MemorySection
    | GlobalSection
    | ExportSection
    | StartSection
    | ElementSection
    | CodeSection
    | DataSection
    deriving (Eq, Show, Enum)

instance Serialize SectionType where
    put section = putWord8 $ fromIntegral $ fromEnum section
    get = do
        op <- fromIntegral `fmap` getWord8
        if op <= fromEnum DataSection
        then return $ toEnum op
        else fail "Unexpected byte in section type position"

instance Serialize ValueType where
    put I32 = putWord8 0x7F
    put I64 = putWord8 0x7E
    put F32 = putWord8 0x7D
    put F64 = putWord8 0x7C

    get = do
        op <- getWord8
        case op of
            0x7F -> return I32
            0x7E -> return I64
            0x7D -> return F32
            0x7C -> return F64
            _ -> fail "unexpected byte in value type position"

instance Serialize FuncType where
    put FuncType {params, results} = do
        putWord8 0x60
        putVec params
        putVec results
    get = do
        byteGuard 0x60
        params <- getVec 
        results <- getVec
        return $ FuncType { params, results }

instance Serialize Module where
    put mod = do
        -- magic
        putWord8 0x00
        putWord8 0x61
        putWord8 0x73
        putWord8 0x6D
        -- version
        putWord8 0x01
        putWord8 0x00
        putWord8 0x00
        putWord8 0x00

        putSection TypeSection $ types mod
    get = undefined