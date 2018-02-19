{-# LANGUAGE NamedFieldPuns #-}

module Language.Wasm.Binary (
    
) where

import Language.Wasm.Structure

import Numeric.Natural (Natural)
import Data.Bits
import Data.Word (Word8)
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding

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

putName :: TL.Text -> Put
putName txt = do
    let bs = TLEncoding.encodeUtf8 txt
    putULEB128 $ fromIntegral $ LBS.length bs
    putLazyByteString bs

getName :: Get TL.Text
getName = do
    len <- getULEB128
    bytes <- getLazyByteString $ fromIntegral len
    return $ TLEncoding.decodeUtf8 bytes

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

instance Serialize ElemType where
    put AnyFunc = putWord8 0x70
    get = byteGuard 0x70 >> return AnyFunc

instance Serialize Limit where
    put (Limit min Nothing) = putWord8 0x00 >> putULEB128 min
    put (Limit min (Just max)) = putWord8 0x01 >> putULEB128 min >> putULEB128 max
    get = do
        op <- getWord8
        case op of
            0x00 -> do
                min <- getULEB128
                return $ Limit min Nothing
            0x01 -> do
                min <- getULEB128
                max <- getULEB128
                return $ Limit min (Just max)
            _ -> fail "Unexpected byte in place of Limit opcode"

instance Serialize TableType where
    put (TableType limit elemType) = do
        put elemType
        put limit
    get = do
        elemType <- get
        limit <- get
        return $ TableType limit elemType

instance Serialize GlobalType where
    put (Const valType) = put valType >> putWord8 0x00
    put (Mut valType) = put valType >> putWord8 0x01
    get = do
        valType <- get
        op <- getWord8
        case op of
            0x00 -> return $ Const valType
            0x01 -> return $ Mut valType
            _ -> fail "Unexpected byte in place of Global type opcode"

instance Serialize ImportDesc where
    put (ImportFunc typeIdx) = putWord8 0x00 >> putULEB128 typeIdx
    put (ImportTable tableType) = putWord8 0x01 >> put tableType
    put (ImportMemory memType) = putWord8 0x02 >> put memType
    put (ImportGlobal globalType) = putWord8 0x03 >> put globalType
    get = do
        op <- getWord8
        case op of
            0x00 -> ImportFunc <$> getULEB128
            0x01 -> ImportTable <$> get
            0x02 -> ImportMemory <$> get
            0x03 -> ImportGlobal <$> get
            _ -> fail "Unexpected byte in place of Import Declaration opcode"

instance Serialize Import where
    put (Import sourceModule name desc) = do
        putName sourceModule
        putName name
        put desc
    get = do
        sourceModule <- getName
        name <- getName
        desc <- get
        return $ Import sourceModule name desc

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
        putSection ImportSection $ imports mod
    get = undefined