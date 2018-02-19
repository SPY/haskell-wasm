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

putResultType :: ResultType -> Put
putResultType [] = putWord8 0x40
putResultType [valType] = put valType
putResultType _ = fail "Current WebAssembly spec does not support returning more then one value"

getResultType :: Get ResultType
getResultType = do
    op <- getWord8
    case op of
        0x40 -> return []
        0x7F -> return [I32]
        0x7E -> return [I64]
        0x7D -> return [F32]
        0x7C -> return [F64]
        _ -> fail "unexpected byte in result type position"

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

instance Serialize Table where
    put (Table tableType) = put tableType
    get = Table <$> get

instance Serialize Memory where
    put (Memory limit) = put limit
    get = Memory <$> get

newtype Index = Index Natural deriving (Show, Eq)

instance Serialize Index where
    put (Index idx) = putULEB128 idx
    get = Index <$> getULEB128

instance Serialize MemArg where
    put (MemArg offset align) = putULEB128 align >> putULEB128 offset
    get = MemArg <$> getULEB128 <*> getULEB128

instance Serialize Instruction where
    put Unreachable = putWord8 0x00
    put Nop = putWord8 0x01
    put (Block result body) = do
        putWord8 0x02
        putResultType result
        mapM put body
        putWord8 0x0B -- END
    put (Loop result body) = do
        putWord8 0x03
        putResultType result
        mapM put body
        putWord8 0x0B -- END
    put If {result, true, false = []} = do
        putWord8 0x04
        putResultType result
        mapM put true
        putWord8 0x0B -- END
    put If {result, true, false} = do
        putWord8 0x04
        putResultType result
        mapM put true
        putWord8 0x05 -- ELSE
        mapM put false
        putWord8 0x0B -- END
    put (Br labelIdx) = putWord8 0x0C >> putULEB128 labelIdx
    put (BrIf labelIdx) = putWord8 0x0D >> putULEB128 labelIdx
    put (BrTable labels label) = putWord8 0x0E >> putVec (map Index labels) >> putULEB128 label
    put Return = putWord8 0x0F
    put (Call funcIdx) = putWord8 0x10 >> putULEB128 funcIdx
    put (CallIndirect typeIdx) = putWord8 0x11 >> putULEB128 typeIdx >> putWord8 0x00
    -- Parametric instructions
    put Drop = putWord8 0x1A
    put Select = putWord8 0x1B
    -- Variable instructions
    put (GetLocal idx) = putWord8 0x20 >> putULEB128 idx
    put (SetLocal idx) = putWord8 0x21 >> putULEB128 idx
    put (TeeLocal idx) = putWord8 0x22 >> putULEB128 idx
    put (GetGlobal idx) = putWord8 0x23 >> putULEB128 idx
    put (SetGlobal idx) = putWord8 0x24 >> putULEB128 idx
    -- Memory instructions
    put (I32Load memArg) = putWord8 0x28 >> put memArg
    put (I64Load memArg) = putWord8 0x29 >> put memArg
    put (F32Load memArg) = putWord8 0x2A >> put memArg
    put (F64Load memArg) = putWord8 0x2B >> put memArg
    put (I32Load8S memArg) = putWord8 0x2C >> put memArg
    put (I32Load8U memArg) = putWord8 0x2D >> put memArg
    put (I32Load16S memArg) = putWord8 0x2E >> put memArg
    put (I32Load16U memArg) = putWord8 0x2F >> put memArg
    put (I64Load8S memArg) = putWord8 0x30 >> put memArg
    put (I64Load8U memArg) = putWord8 0x31 >> put memArg
    put (I64Load16S memArg) = putWord8 0x32 >> put memArg
    put (I64Load16U memArg) = putWord8 0x33 >> put memArg
    put (I64Load32S memArg) = putWord8 0x34 >> put memArg
    put (I64Load32U memArg) = putWord8 0x35 >> put memArg
    put (I32Store memArg) = putWord8 0x36 >> put memArg
    put (I64Store memArg) = putWord8 0x37 >> put memArg
    put (F32Store memArg) = putWord8 0x38 >> put memArg
    put (F64Store memArg) = putWord8 0x39 >> put memArg
    put (I32Store8 memArg) = putWord8 0x3A >> put memArg
    put (I32Store16 memArg) = putWord8 0x3B >> put memArg
    put (I64Store8 memArg) = putWord8 0x3C >> put memArg
    put (I64Store16 memArg) = putWord8 0x3D >> put memArg
    put (I64Store32 memArg) = putWord8 0x3E >> put memArg
    put CurrentMemory = putWord8 0x3F >> putWord8 0x00
    put GrowMemory = putWord8 0x40 >> putWord8 0x00
    -- Numeric instructions
    put (I32Const val) = putWord8 0x41 -- TODO: PACK VALUE AS SIGNED LEB128
    put (I64Const val) = putWord8 0x42 -- TODO: PACK VALUE AS SIGNED LEB128
    put (F32Const val) = putWord8 0x43 >> putFloat32le val
    put (F64Const val) = putWord8 0x44 >> putFloat64le val
    put I32Eqz = putWord8 0x45
    put (IRelOp BS32 IEq) = putWord8 0x46
    put (IRelOp BS32 INe) = putWord8 0x47
    put (IRelOp BS32 ILtS) = putWord8 0x48
    put (IRelOp BS32 ILtU) = putWord8 0x49
    put (IRelOp BS32 IGtS) = putWord8 0x4A
    put (IRelOp BS32 IGtU) = putWord8 0x4B
    put (IRelOp BS32 ILeS) = putWord8 0x4C
    put (IRelOp BS32 ILeU) = putWord8 0x4D
    put (IRelOp BS32 IGeS) = putWord8 0x4E
    put (IRelOp BS32 IGeU) = putWord8 0x4F
    put I64Eqz = putWord8 0x50
    put (IRelOp BS64 IEq) = putWord8 0x51
    put (IRelOp BS64 INe) = putWord8 0x52
    put (IRelOp BS64 ILtS) = putWord8 0x53
    put (IRelOp BS64 ILtU) = putWord8 0x54
    put (IRelOp BS64 IGtS) = putWord8 0x55
    put (IRelOp BS64 IGtU) = putWord8 0x56
    put (IRelOp BS64 ILeS) = putWord8 0x57
    put (IRelOp BS64 ILeU) = putWord8 0x58
    put (IRelOp BS64 IGeS) = putWord8 0x59
    put (IRelOp BS64 IGeU) = putWord8 0x5A
    put (FRelOp BS32 FEq) = putWord8 0x5B
    put (FRelOp BS32 FNe) = putWord8 0x5C
    put (FRelOp BS32 FLt) = putWord8 0x5D
    put (FRelOp BS32 FGt) = putWord8 0x5E
    put (FRelOp BS32 FLe) = putWord8 0x5F
    put (FRelOp BS32 FGe) = putWord8 0x60
    put (FRelOp BS64 FEq) = putWord8 0x61
    put (FRelOp BS64 FNe) = putWord8 0x62
    put (FRelOp BS64 FLt) = putWord8 0x63
    put (FRelOp BS64 FGt) = putWord8 0x64
    put (FRelOp BS64 FLe) = putWord8 0x65
    put (FRelOp BS64 FGe) = putWord8 0x66
    put (IUnOp BS32 IClz) = putWord8 0x67
    put (IUnOp BS32 ICtz) = putWord8 0x68
    put (IUnOp BS32 IPopcnt) = putWord8 0x69
    put (IBinOp BS32 IAdd) = putWord8 0x6A
    put (IBinOp BS32 ISub) = putWord8 0x6B
    put (IBinOp BS32 IMul) = putWord8 0x6C
    put (IBinOp BS32 IDivS) = putWord8 0x6D
    put (IBinOp BS32 IDivU) = putWord8 0x6E
    put (IBinOp BS32 IRemS) = putWord8 0x6F
    put (IBinOp BS32 IRemU) = putWord8 0x70
    put (IBinOp BS32 IAnd) = putWord8 0x71
    put (IBinOp BS32 IOr) = putWord8 0x72
    put (IBinOp BS32 IXor) = putWord8 0x73
    put (IBinOp BS32 IShl) = putWord8 0x74
    put (IBinOp BS32 IShrS) = putWord8 0x75
    put (IBinOp BS32 IShrU) = putWord8 0x76
    put (IBinOp BS32 IRotl) = putWord8 0x77
    put (IBinOp BS32 IRotr) = putWord8 0x78
    put (IUnOp BS64 IClz) = putWord8 0x79
    put (IUnOp BS64 ICtz) = putWord8 0x7A
    put (IUnOp BS64 IPopcnt) = putWord8 0x7B
    put (IBinOp BS64 IAdd) = putWord8 0x7C
    put (IBinOp BS64 ISub) = putWord8 0x7D
    put (IBinOp BS64 IMul) = putWord8 0x7E
    put (IBinOp BS64 IDivS) = putWord8 0x7F
    put (IBinOp BS64 IDivU) = putWord8 0x80
    put (IBinOp BS64 IRemS) = putWord8 0x81
    put (IBinOp BS64 IRemU) = putWord8 0x82
    put (IBinOp BS64 IAnd) = putWord8 0x83
    put (IBinOp BS64 IOr) = putWord8 0x84
    put (IBinOp BS64 IXor) = putWord8 0x85
    put (IBinOp BS64 IShl) = putWord8 0x86
    put (IBinOp BS64 IShrS) = putWord8 0x87
    put (IBinOp BS64 IShrU) = putWord8 0x88
    put (IBinOp BS64 IRotl) = putWord8 0x89
    put (IBinOp BS64 IRotr) = putWord8 0x8A
    put (FUnOp BS32 FAbs) = putWord8 0x8B
    put (FUnOp BS32 FNeg) = putWord8 0x8C
    put (FUnOp BS32 FCeil) = putWord8 0x8D
    put (FUnOp BS32 FFloor) = putWord8 0x8E
    put (FUnOp BS32 FTrunc) = putWord8 0x8F
    put (FUnOp BS32 FNearest) = putWord8 0x90
    put (FUnOp BS32 FSqrt) = putWord8 0x91
    put (FBinOp BS32 FAdd) = putWord8 0x92
    put (FBinOp BS32 FSub) = putWord8 0x93
    put (FBinOp BS32 FMul) = putWord8 0x94
    put (FBinOp BS32 FDiv) = putWord8 0x95
    put (FBinOp BS32 FMin) = putWord8 0x96
    put (FBinOp BS32 FMax) = putWord8 0x97
    put (FBinOp BS32 FCopySign) = putWord8 0x98
    put (FUnOp BS64 FAbs) = putWord8 0x99
    put (FUnOp BS64 FNeg) = putWord8 0x9A
    put (FUnOp BS64 FCeil) = putWord8 0x9B
    put (FUnOp BS64 FFloor) = putWord8 0x9C
    put (FUnOp BS64 FTrunc) = putWord8 0x9D
    put (FUnOp BS64 FNearest) = putWord8 0x9E
    put (FUnOp BS64 FSqrt) = putWord8 0x9F
    put (FBinOp BS64 FAdd) = putWord8 0xA0
    put (FBinOp BS64 FSub) = putWord8 0xA1
    put (FBinOp BS64 FMul) = putWord8 0xA2
    put (FBinOp BS64 FDiv) = putWord8 0xA3
    put (FBinOp BS64 FMin) = putWord8 0xA4
    put (FBinOp BS64 FMax) = putWord8 0xA5
    put (FBinOp BS64 FCopySign) = putWord8 0xA6
    put I32WrapI64 = putWord8 0xA7
    put (ITruncFS BS32 BS32) = putWord8 0xA8
    put (ITruncFU BS32 BS32) = putWord8 0xA9
    put (ITruncFS BS32 BS64) = putWord8 0xAA
    put (ITruncFU BS32 BS64) = putWord8 0xAB
    put I64ExtendSI32 = putWord8 0xAC
    put I64ExtendUI32 = putWord8 0xAD
    put (ITruncFS BS64 BS32) = putWord8 0xAE
    put (ITruncFU BS64 BS32) = putWord8 0xAF
    put (ITruncFS BS64 BS64) = putWord8 0xB0
    put (ITruncFU BS64 BS64) = putWord8 0xB1
    put (FConvertIS BS32 BS32) = putWord8 0xB2
    put (FConvertIU BS32 BS32) = putWord8 0xB3
    put (FConvertIS BS32 BS64) = putWord8 0xB4
    put (FConvertIU BS32 BS64) = putWord8 0xB4
    put F32DemoteF64 = putWord8 0xB6
    put (FConvertIS BS64 BS32) = putWord8 0xB7
    put (FConvertIU BS64 BS32) = putWord8 0xB8
    put (FConvertIS BS64 BS64) = putWord8 0xB9
    put (FConvertIU BS64 BS64) = putWord8 0xBA
    put F64PromoteF32 = putWord8 0xBB
    put (IReinterpretF BS32) = putWord8 0xBC
    put (IReinterpretF BS64) = putWord8 0xBD
    put (FReinterpretI BS32) = putWord8 0xBE
    put (FReinterpretI BS64) = putWord8 0xBF

    get = undefined

instance Serialize Global where
    put (Global globalType expr) = do
        put globalType
        mapM put expr
        putWord8 0x0B -- END
    get = undefined -- Global <$> get <*> get

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
        putSection FunctionSection $ map funcType $ functions mod
        putSection TableSection $ tables mod
        putSection MemorySection $ mems mod
        putSection GlobalSection $ globals mod
    get = undefined