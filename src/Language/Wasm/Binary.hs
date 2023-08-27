{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Wasm.Binary (
    dumpModule,
    dumpModuleLazy,
    decodeModule,
    decodeModuleLazy
) where

import Language.Wasm.Structure

import Numeric.Natural (Natural)
import Data.Bits
import Data.Word (Word8, Word32, Word64)
import Data.Int (Int8, Int32, Int64)
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEncoding

asInt32 :: Word32 -> Int32
asInt32 w =
    if w < 0x80000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFF - w + 1)

asInt64 :: Word64 -> Int64
asInt64 w =
    if w < 0x8000000000000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFFFFFFFFFF - w + 1)

getULEB128 :: (Integral a, Bits a) => Int -> Get a
getULEB128 bitsBudget = do
    if bitsBudget > 0 then return () else fail "integer representation too long"
    val <- getWord8
    if bitsBudget >= 7 || val .&. 0x7F < 1 `shiftL` bitsBudget then return () else fail "integer too large"
    if not (testBit val 7)
    then return $ fromIntegral val
    else do
        rest <- getULEB128 (bitsBudget - 7)
        return $ (fromIntegral $ val .&. 0x7F) .|. (rest `shiftL` 7)

putULEB128 :: (Integral a, Bits a) => a -> Put
putULEB128 val =
    if val < 128
    then putWord8 $ fromIntegral val
    else do
        putWord8 $ 0x80 + (0x7F .&. fromIntegral val)
        putULEB128 $ val `shiftR` 7

getSLEB128 :: (Integral a, Bits a) => Int -> Get a
getSLEB128 bitsBudget = do
    if bitsBudget > 0 then return () else fail "integer representation too long"
    let toInt8 :: Word8 -> Int8
        toInt8 = fromIntegral
    a <- getWord8
    let mask = (0xFF `shiftL` (bitsBudget - 1)) .&. 0x7F
    if bitsBudget >= 7 || a .&. mask == 0 || a .&. mask == mask then return () else fail "integer too large"
    if not (testBit a 7)
    then return . fromIntegral . toInt8 $ (a .&. 0x7f) .|. ((a .&. 0x40) `shiftL` 1)
    else do
        b <- getSLEB128 (bitsBudget - 7)
        return $ (b `shiftL` 7) .|. (fromIntegral (a .&. 0x7f))

putSLEB128 :: (Integral a, Bits a) => a -> Put
putSLEB128 a = go a
    where
        ext = if a >= 0 then 0 else complement 0
        go x = do
            let 
                r = x `shiftR` 7
                w = x .&. 0x7f
            if r /= ext
            then do
                putWord8 (fromIntegral w .|. 0x80)
                go r
            else
                if (testBit w 6 && a < 0) || (not (testBit w 6) && a >= 0)
                then putWord8 (fromIntegral w)
                else do
                    putWord8 (fromIntegral w .|. 0x80)
                    putWord8 (fromIntegral ext .&. 0x7F)

putVec :: Serialize a => [a] -> Put
putVec list = do
    putULEB128 $ length list
    mapM put list
    return ()

getVec :: Serialize a => Get [a]
getVec = do
    len <- getULEB128 32
    sequence $ replicate len get

byteGuard :: Word8 -> Get ()
byteGuard expected = do
    byte <- getWord8
    if byte == expected
    then return ()
    else fail $ "Expected " ++ show expected ++ ", but encountered " ++ show byte

putSection :: SectionType -> Put -> Put
putSection section content = do
    put section
    let payload = runPut content
    putULEB128 $ BS.length payload
    putByteString payload

skipCustomSection :: Get ()
skipCustomSection = do
    byteGuard 0x00
    size <- getULEB128 32
    content <- getByteString size
    case runGet getName content of
        Right _name -> return ()
        Left _ -> fail "invalid UTF-8 encoding"

getSection :: SectionType -> Get a -> a -> Get a
getSection sectionType parser def = do
    empty <- isEmpty
    if empty
    then return def
    else do
        nextByte <- lookAhead getWord8
        parseSection $ fromIntegral nextByte
    where
        parseSection op
            | op == 0 = skipCustomSection >> getSection sectionType parser def
            | op == fromEnum sectionType = do
                getWord8
                len <- getULEB128 32
                isolate len parser
            | op > fromEnum DataSection = fail "invalid section id"
            | op > fromEnum sectionType = return def
            | otherwise =
                fail $ "Incorrect order of sections. Expected " ++ show sectionType
                    ++ ", but found " ++ show (toEnum op :: SectionType)

putName :: TL.Text -> Put
putName txt = do
    let bs = TLEncoding.encodeUtf8 txt
    putULEB128 $ LBS.length bs
    putLazyByteString bs

getName :: Get TL.Text
getName = do
    len <- getULEB128 32
    bytes <- getLazyByteString len
    case TLEncoding.decodeUtf8' bytes of
        Right name -> return name
        Left _ -> fail "invalid UTF-8 encoding"

putResultType :: ResultType -> Put
putResultType [] = putWord8 0x40
putResultType [valType] = put valType
putResultType _ = error "Current WebAssembly spec does not support returning more then one value"

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

putBlockType :: BlockType -> Put
putBlockType (Inline Nothing) = putWord8 0x40
putBlockType (Inline (Just valType)) = put valType
putBlockType (TypeIndex idx) = putSLEB128 idx

getInlineBlockType :: Get (Maybe (Maybe ValueType))
getInlineBlockType = do
    op <- getWord8
    case op of
        0x40 -> return $ Just Nothing
        0x7F -> return $ Just (Just I32)
        0x7E -> return $ Just (Just I64)
        0x7D -> return $ Just (Just F32)
        0x7C -> return $ Just (Just F64)
        _ -> return Nothing

getBlockType :: Get BlockType
getBlockType = do
    inlineType <- lookAheadM getInlineBlockType
    case inlineType of
        Just inline -> return $ Inline inline
        Nothing -> TypeIndex <$> getSLEB128 33

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
    put FuncRef = putWord8 0x70
    put ExternRef = putWord8 0x6F
    get = do
        op <- getWord8
        case op of
            0x70 -> return FuncRef
            0x69 -> return ExternRef
            _ -> fail "unknown reference type"

instance Serialize Limit where
    put (Limit min Nothing) = putWord8 0x00 >> putULEB128 min
    put (Limit min (Just max)) = putWord8 0x01 >> putULEB128 min >> putULEB128 max
    get = do
        op <- getWord8
        case op of
            0x00 -> do
                min <- getULEB128 32
                return $ Limit min Nothing
            0x01 -> do
                min <- getULEB128 32
                max <- getULEB128 32
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
            _ -> fail "invalid mutability"

instance Serialize ImportDesc where
    put (ImportFunc typeIdx) = putWord8 0x00 >> putULEB128 typeIdx
    put (ImportTable tableType) = putWord8 0x01 >> put tableType
    put (ImportMemory memType) = putWord8 0x02 >> put memType
    put (ImportGlobal globalType) = putWord8 0x03 >> put globalType
    get = do
        op <- getWord8
        case op of
            0x00 -> ImportFunc <$> getULEB128 32
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

newtype Index = Index { unIndex :: Natural } deriving (Show, Eq)

instance Serialize Index where
    put (Index idx) = putULEB128 idx
    get = Index <$> getULEB128 32

newtype Expr = Expr { unExpr :: Expression } deriving (Show, Eq)

instance Serialize Expr where
    put (Expr expr) = putExpression expr
    get = Expr <$> getExpression

instance Serialize MemArg where
    put MemArg { align, offset } = putULEB128 align >> putULEB128 offset
    get = do
        align <- getULEB128 32
        offset <- getULEB128 32
        return $ MemArg { align, offset }

instance Serialize (Instruction Natural) where
    put Unreachable = putWord8 0x00
    put Nop = putWord8 0x01
    put (Block blockType body) = do
        putWord8 0x02
        putBlockType blockType
        putExpression body
    put (Loop blockType body) = do
        putWord8 0x03
        putBlockType blockType
        putExpression body
    put If {blockType, true, false = []} = do
        putWord8 0x04
        putBlockType blockType
        putExpression true
    put If {blockType, true, false} = do
        putWord8 0x04
        putBlockType blockType
        mapM_ put true
        putWord8 0x05 -- ELSE
        putExpression false
    put (Br labelIdx) = putWord8 0x0C >> putULEB128 labelIdx
    put (BrIf labelIdx) = putWord8 0x0D >> putULEB128 labelIdx
    put (BrTable labels label) = putWord8 0x0E >> putVec (map Index labels) >> putULEB128 label
    put Return = putWord8 0x0F
    put (Call funcIdx) = putWord8 0x10 >> putULEB128 funcIdx
    put (CallIndirect tableIdx typeIdx) = putWord8 0x11 >> putULEB128 typeIdx >> putULEB128 tableIdx
    -- Reference instructions
    put (RefNull refType) = putWord8 0xD0 >> put refType
    put RefIsNull = putWord8 0xD1
    put (RefFunc index) = putWord8 0xD2 >> putULEB128 index
    -- Parametric instructions
    put Drop = putWord8 0x1A
    put (Select Nothing) = putWord8 0x1B
    put (Select (Just types)) = putWord8 0x1C >> putVec types
    -- Variable instructions
    put (GetLocal idx) = putWord8 0x20 >> putULEB128 idx
    put (SetLocal idx) = putWord8 0x21 >> putULEB128 idx
    put (TeeLocal idx) = putWord8 0x22 >> putULEB128 idx
    put (GetGlobal idx) = putWord8 0x23 >> putULEB128 idx
    put (SetGlobal idx) = putWord8 0x24 >> putULEB128 idx
    -- Table instructions
    put (TableGet idx) = putWord8 0x25 >> putULEB128 idx
    put (TableSet idx) = putWord8 0x26 >> putULEB128 idx
    put (TableInit tableIdx elemIdx) = do
        putWord8 0xFC
        putULEB128 (0x0C :: Word32)
        putULEB128 tableIdx
        putULEB128 elemIdx
    put (ElemDrop elemIdx) = do
        putWord8 0xFC
        putULEB128 (0x0D :: Word32)
        putULEB128 elemIdx
    put (TableCopy fromIdx toIdx) = do
        putWord8 0xFC
        putULEB128 (0x0E :: Word32)
        putULEB128 fromIdx
        putULEB128 toIdx
    put (TableGrow tableIdx) = do
        putWord8 0xFC
        putULEB128 (0x0F :: Word32)
        putULEB128 tableIdx
    put (TableSize tableIdx) = do
        putWord8 0xFC
        putULEB128 (0x10 :: Word32)
        putULEB128 tableIdx
    put (TableFill tableIdx) = do
        putWord8 0xFC
        putULEB128 (0x11 :: Word32)
        putULEB128 tableIdx
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
    put MemorySize = putWord8 0x3F >> putWord8 0x00
    put MemoryGrow = putWord8 0x40 >> putWord8 0x00
    put (MemoryInit dataIdx) = do
        putWord8 0xFC
        putULEB128 (0x08 :: Word32)
        putULEB128 dataIdx
        putWord8 0
    put (DataDrop dataIdx) = do
        putWord8 0xFC
        putULEB128 (0x09 :: Word32)
        putULEB128 dataIdx
    put MemoryCopy = do
        putWord8 0xFC
        putULEB128 (0x0A :: Word32)
        putWord8 0
        putWord8 0
    put MemoryFill = do
        putWord8 0xFC
        putULEB128 (0x0B :: Word32)
        putWord8 0
    -- Numeric instructions
    put (I32Const val) = putWord8 0x41 >> putSLEB128 (asInt32 val)
    put (I64Const val) = putWord8 0x42 >> putSLEB128 (asInt64 val)
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
    put (FConvertIU BS32 BS64) = putWord8 0xB5
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

    put (IUnOp BS32 IExtend8S) = putWord8 0xC0
    put (IUnOp BS32 IExtend16S) = putWord8 0xC1
    put (IUnOp BS32 IExtend32S) = error "Opcode for i32.extend32_s doesn't exist"
    put (IUnOp BS64 IExtend8S) = putWord8 0xC2
    put (IUnOp BS64 IExtend16S) = putWord8 0xC3
    put (IUnOp BS64 IExtend32S) = putWord8 0xC4

    put (ITruncSatFS BS32 BS32) = putWord8 0xFC >> putULEB128 (0x00 :: Word32)
    put (ITruncSatFU BS32 BS32) = putWord8 0xFC >> putULEB128 (0x01 :: Word32)
    put (ITruncSatFS BS32 BS64) = putWord8 0xFC >> putULEB128 (0x02 :: Word32)
    put (ITruncSatFU BS32 BS64) = putWord8 0xFC >> putULEB128 (0x03 :: Word32)
    put (ITruncSatFS BS64 BS32) = putWord8 0xFC >> putULEB128 (0x04 :: Word32)
    put (ITruncSatFU BS64 BS32) = putWord8 0xFC >> putULEB128 (0x05 :: Word32)
    put (ITruncSatFS BS64 BS64) = putWord8 0xFC >> putULEB128 (0x06 :: Word32)
    put (ITruncSatFU BS64 BS64) = putWord8 0xFC >> putULEB128 (0x07 :: Word32)

    get = do
        op <- getWord8
        case op of
            0x00 -> return Unreachable
            0x01 -> return Nop
            0x02 -> Block <$> getBlockType <*> getExpression
            0x03 -> Loop <$> getBlockType <*> getExpression
            0x04 -> do
                blockType <- getBlockType
                (true, hasElse) <- getTrueBranch
                false <- if hasElse then getExpression else return []
                return $ If blockType true false
            0x0C -> Br <$> getULEB128 32
            0x0D -> BrIf <$> getULEB128 32
            0x0E -> BrTable <$> (map unIndex <$> getVec) <*> getULEB128 32
            0x0F -> return $ Return
            0x10 -> Call <$> getULEB128 32
            0x11 -> do
                typeIdx <- getULEB128 32
                tableIdx <- getULEB128 32
                return $ CallIndirect tableIdx typeIdx
            -- Reference instructions
            0xD0 -> RefNull <$> get
            0xD1 -> return RefIsNull
            0xD2 -> RefFunc <$> getULEB128 32
            -- Parametric instructions
            0x1A -> return $ Drop
            0x1B -> return $ Select Nothing
            -- Variable instructions
            0x20 -> GetLocal <$> getULEB128 32
            0x21 -> SetLocal <$> getULEB128 32
            0x22 -> TeeLocal <$> getULEB128 32
            0x23 -> GetGlobal <$> getULEB128 32
            0x24 -> SetGlobal <$> getULEB128 32
            -- Memory instructions
            0x28 -> I32Load <$> get
            0x29 -> I64Load <$> get
            0x2A -> F32Load <$> get
            0x2B -> F64Load <$> get
            0x2C -> I32Load8S <$> get
            0x2D -> I32Load8U <$> get
            0x2E -> I32Load16S <$> get
            0x2F -> I32Load16U <$> get
            0x30 -> I64Load8S <$> get
            0x31 -> I64Load8U <$> get
            0x32 -> I64Load16S <$> get
            0x33 -> I64Load16U <$> get
            0x34 -> I64Load32S <$> get
            0x35 -> I64Load32U <$> get
            0x36 -> I32Store <$> get
            0x37 -> I64Store <$> get
            0x38 -> F32Store <$> get
            0x39 -> F64Store <$> get
            0x3A -> I32Store8 <$> get
            0x3B -> I32Store16 <$> get
            0x3C -> I64Store8 <$> get
            0x3D -> I64Store16 <$> get
            0x3E -> I64Store32 <$> get
            0x3F -> byteGuard 0x00 >> (return $ MemorySize)
            0x40 -> byteGuard 0x00 >> (return $ MemoryGrow)
            -- Numeric instructions
            0x41 -> I32Const <$> getSLEB128 32
            0x42 -> I64Const <$> getSLEB128 64
            0x43 -> F32Const <$> getFloat32le
            0x44 -> F64Const <$> getFloat64le
            0x45 -> return $ I32Eqz
            0x46 -> return $ IRelOp BS32 IEq
            0x47 -> return $ IRelOp BS32 INe
            0x48 -> return $ IRelOp BS32 ILtS
            0x49 -> return $ IRelOp BS32 ILtU
            0x4A -> return $ IRelOp BS32 IGtS
            0x4B -> return $ IRelOp BS32 IGtU
            0x4C -> return $ IRelOp BS32 ILeS
            0x4D -> return $ IRelOp BS32 ILeU
            0x4E -> return $ IRelOp BS32 IGeS
            0x4F -> return $ IRelOp BS32 IGeU
            0x50 -> return $ I64Eqz
            0x51 -> return $ IRelOp BS64 IEq
            0x52 -> return $ IRelOp BS64 INe
            0x53 -> return $ IRelOp BS64 ILtS
            0x54 -> return $ IRelOp BS64 ILtU
            0x55 -> return $ IRelOp BS64 IGtS
            0x56 -> return $ IRelOp BS64 IGtU
            0x57 -> return $ IRelOp BS64 ILeS
            0x58 -> return $ IRelOp BS64 ILeU
            0x59 -> return $ IRelOp BS64 IGeS
            0x5A -> return $ IRelOp BS64 IGeU
            0x5B -> return $ FRelOp BS32 FEq
            0x5C -> return $ FRelOp BS32 FNe
            0x5D -> return $ FRelOp BS32 FLt
            0x5E -> return $ FRelOp BS32 FGt
            0x5F -> return $ FRelOp BS32 FLe
            0x60 -> return $ FRelOp BS32 FGe
            0x61 -> return $ FRelOp BS64 FEq
            0x62 -> return $ FRelOp BS64 FNe
            0x63 -> return $ FRelOp BS64 FLt
            0x64 -> return $ FRelOp BS64 FGt
            0x65 -> return $ FRelOp BS64 FLe
            0x66 -> return $ FRelOp BS64 FGe
            0x67 -> return $ IUnOp BS32 IClz
            0x68 -> return $ IUnOp BS32 ICtz
            0x69 -> return $ IUnOp BS32 IPopcnt
            0x6A -> return $ IBinOp BS32 IAdd
            0x6B -> return $ IBinOp BS32 ISub
            0x6C -> return $ IBinOp BS32 IMul
            0x6D -> return $ IBinOp BS32 IDivS
            0x6E -> return $ IBinOp BS32 IDivU
            0x6F -> return $ IBinOp BS32 IRemS
            0x70 -> return $ IBinOp BS32 IRemU
            0x71 -> return $ IBinOp BS32 IAnd
            0x72 -> return $ IBinOp BS32 IOr
            0x73 -> return $ IBinOp BS32 IXor
            0x74 -> return $ IBinOp BS32 IShl
            0x75 -> return $ IBinOp BS32 IShrS
            0x76 -> return $ IBinOp BS32 IShrU
            0x77 -> return $ IBinOp BS32 IRotl
            0x78 -> return $ IBinOp BS32 IRotr
            0x79 -> return $ IUnOp BS64 IClz
            0x7A -> return $ IUnOp BS64 ICtz
            0x7B -> return $ IUnOp BS64 IPopcnt
            0x7C -> return $ IBinOp BS64 IAdd
            0x7D -> return $ IBinOp BS64 ISub
            0x7E -> return $ IBinOp BS64 IMul
            0x7F -> return $ IBinOp BS64 IDivS
            0x80 -> return $ IBinOp BS64 IDivU
            0x81 -> return $ IBinOp BS64 IRemS
            0x82 -> return $ IBinOp BS64 IRemU
            0x83 -> return $ IBinOp BS64 IAnd
            0x84 -> return $ IBinOp BS64 IOr
            0x85 -> return $ IBinOp BS64 IXor
            0x86 -> return $ IBinOp BS64 IShl
            0x87 -> return $ IBinOp BS64 IShrS
            0x88 -> return $ IBinOp BS64 IShrU
            0x89 -> return $ IBinOp BS64 IRotl
            0x8A -> return $ IBinOp BS64 IRotr
            0x8B -> return $ FUnOp BS32 FAbs
            0x8C -> return $ FUnOp BS32 FNeg
            0x8D -> return $ FUnOp BS32 FCeil
            0x8E -> return $ FUnOp BS32 FFloor
            0x8F -> return $ FUnOp BS32 FTrunc
            0x90 -> return $ FUnOp BS32 FNearest
            0x91 -> return $ FUnOp BS32 FSqrt
            0x92 -> return $ FBinOp BS32 FAdd
            0x93 -> return $ FBinOp BS32 FSub
            0x94 -> return $ FBinOp BS32 FMul
            0x95 -> return $ FBinOp BS32 FDiv
            0x96 -> return $ FBinOp BS32 FMin
            0x97 -> return $ FBinOp BS32 FMax
            0x98 -> return $ FBinOp BS32 FCopySign
            0x99 -> return $ FUnOp BS64 FAbs
            0x9A -> return $ FUnOp BS64 FNeg
            0x9B -> return $ FUnOp BS64 FCeil
            0x9C -> return $ FUnOp BS64 FFloor
            0x9D -> return $ FUnOp BS64 FTrunc
            0x9E -> return $ FUnOp BS64 FNearest
            0x9F -> return $ FUnOp BS64 FSqrt
            0xA0 -> return $ FBinOp BS64 FAdd
            0xA1 -> return $ FBinOp BS64 FSub
            0xA2 -> return $ FBinOp BS64 FMul
            0xA3 -> return $ FBinOp BS64 FDiv
            0xA4 -> return $ FBinOp BS64 FMin
            0xA5 -> return $ FBinOp BS64 FMax
            0xA6 -> return $ FBinOp BS64 FCopySign
            0xA7 -> return $ I32WrapI64
            0xA8 -> return $ ITruncFS BS32 BS32
            0xA9 -> return $ ITruncFU BS32 BS32
            0xAA -> return $ ITruncFS BS32 BS64
            0xAB -> return $ ITruncFU BS32 BS64
            0xAC -> return $ I64ExtendSI32
            0xAD -> return $ I64ExtendUI32
            0xAE -> return $ ITruncFS BS64 BS32
            0xAF -> return $ ITruncFU BS64 BS32
            0xB0 -> return $ ITruncFS BS64 BS64
            0xB1 -> return $ ITruncFU BS64 BS64
            0xB2 -> return $ FConvertIS BS32 BS32
            0xB3 -> return $ FConvertIU BS32 BS32
            0xB4 -> return $ FConvertIS BS32 BS64
            0xB5 -> return $ FConvertIU BS32 BS64
            0xB6 -> return $ F32DemoteF64
            0xB7 -> return $ FConvertIS BS64 BS32
            0xB8 -> return $ FConvertIU BS64 BS32
            0xB9 -> return $ FConvertIS BS64 BS64
            0xBA -> return $ FConvertIU BS64 BS64
            0xBB -> return $ F64PromoteF32
            0xBC -> return $ IReinterpretF BS32
            0xBD -> return $ IReinterpretF BS64
            0xBE -> return $ FReinterpretI BS32
            0xBF -> return $ FReinterpretI BS64
            0xC0 -> return $ IUnOp BS32 IExtend8S
            0xC1 -> return $ IUnOp BS32 IExtend16S
            0xC2 -> return $ IUnOp BS64 IExtend8S
            0xC3 -> return $ IUnOp BS64 IExtend16S
            0xC4 -> return $ IUnOp BS64 IExtend32S
            0xFC -> do -- misc
                ext <- getULEB128 32
                case (ext :: Word32) of
                    0x00 -> return $ ITruncSatFS BS32 BS32
                    0x01 -> return $ ITruncSatFU BS32 BS32
                    0x02 -> return $ ITruncSatFS BS32 BS64
                    0x03 -> return $ ITruncSatFU BS32 BS64
                    0x04 -> return $ ITruncSatFS BS64 BS32
                    0x05 -> return $ ITruncSatFU BS64 BS32
                    0x06 -> return $ ITruncSatFS BS64 BS64
                    0x07 -> return $ ITruncSatFU BS64 BS64
                    _ -> fail "Unknown byte value after misc instruction byte"
            byte -> fail $ "Unknown byte value in place of instruction opcode: " ++ (show byte)

putExpression :: Expression -> Put
putExpression expr = do
    mapM_ put expr
    putWord8 0x0B -- END

getExpression :: Get Expression
getExpression = go []
    where
        go :: Expression -> Get Expression
        go acc = do
            nextByte <- lookAhead getWord8
            if nextByte == 0x0B -- END OF EXPR
            then getWord8 >> (return $ reverse acc)
            else get >>= \instr -> go (instr : acc)

getTrueBranch :: Get (Expression, Bool)
getTrueBranch = go []
    where
        go :: Expression -> Get (Expression, Bool)
        go acc = do
            nextByte <- lookAhead getWord8
            case nextByte of
                -- END OF EXPR
                0x0B -> getWord8 >> (return $ (reverse acc, False))
                -- ELSE 
                0x05 -> getWord8 >> (return $ (reverse acc, True))
                _ -> get >>= \instr -> go (instr : acc)

instance Serialize Global where
    put (Global globalType expr) = do
        put globalType
        putExpression expr
    get = Global <$> get <*> getExpression

instance Serialize ExportDesc where
    put (ExportFunc idx) = putWord8 0x00 >> putULEB128 idx
    put (ExportTable idx) = putWord8 0x01 >> putULEB128 idx
    put (ExportMemory idx) = putWord8 0x02 >> putULEB128 idx
    put (ExportGlobal idx) = putWord8 0x03 >> putULEB128 idx
    get = do
        op <- getWord8
        idx <- getULEB128 32
        case op of
            0x00 -> return $ ExportFunc idx
            0x01 -> return $ ExportTable idx
            0x02 -> return $ ExportMemory idx
            0x03 -> return $ ExportGlobal idx
            _ -> fail "Unexpected byte value in position of Export Description opcode"

instance Serialize Export where
    put (Export name desc) = do
        putName name
        put desc
    get = Export <$> getName <*> get

instance Serialize ElemSegment where
    put (ElemSegment elemType Passive elements) = do
        putWord8 0x05
        put elemType
        putVec $ map Expr elements
    put (ElemSegment elemType (Active tableIndex offset) elements) = do
        putWord8 0x06
        putULEB128 tableIndex
        putExpression offset
        put elemType
        putVec $ map Expr elements
    put (ElemSegment elemType Declarative elements) = do
        putWord8 0x07
        put elemType
        putVec $ map Expr elements

    get = do
        let funcIndexes = map ((:[]) . RefFunc . unIndex) <$> getVec
        let elemKind = byteGuard 0x00 >> return FuncRef
        op <- getULEB128 32
        case (op :: Word8) of
            0x00 -> do
                offset <- getExpression
                ElemSegment FuncRef (Active 0 offset) <$> funcIndexes
            0x01 -> do
                elemType <- elemKind
                ElemSegment elemType Passive <$> funcIndexes
            0x02 -> do
                tableIndex <- getULEB128 32
                offset <- getExpression
                elemType <- elemKind
                ElemSegment elemType (Active tableIndex offset) <$> funcIndexes
            0x03 -> do
                elemType <- elemKind
                ElemSegment elemType Declarative <$> funcIndexes
            0x04 -> do
                offset <- getExpression
                ElemSegment FuncRef (Active 0 offset) <$> funcIndexes
            0x05 -> do
                elemType <- get
                ElemSegment elemType Passive . map unExpr <$> getVec
            0x06 -> do
                tableIndex <- getULEB128 32
                offset <- getExpression
                elemType <- get
                ElemSegment elemType (Active tableIndex offset) <$> getVec
            0x07 -> do
                elemType <- get
                ElemSegment elemType Declarative <$> getVec
            _ ->
                fail "unknown element segment type"

data LocalTypeRange = LocalTypeRange Natural ValueType deriving (Show, Eq)

instance Serialize LocalTypeRange where
    put (LocalTypeRange len valType) = do
        putULEB128 len
        put valType
    get = LocalTypeRange <$> getULEB128 32 <*> get

instance Serialize Function where
    put Function {localTypes = locals, body} = do
        let bs = runPut $ do
                putVec $ map (LocalTypeRange 1) locals
                putExpression body
        putULEB128 $ BS.length bs
        putByteString bs
    get = do
        _size <- getULEB128 32 :: Get Natural
        localRanges <- getVec
        let localLen = sum $ map (\(LocalTypeRange n _) -> n) localRanges
        if localLen < 2^32 then return () else fail "too many locals"
        let locals = concat $ map (\(LocalTypeRange n val) -> replicate (fromIntegral n) val) localRanges 
        body <- getExpression
        return $ Function 0 locals body

instance Serialize DataSegment where
    put (DataSegment (ActiveData memIdx offset) init) = do
        putWord8 0x02
        putULEB128 memIdx
        putExpression offset
        putULEB128 $ LBS.length init
        putLazyByteString init
    put (DataSegment PassiveData init) = do
        putWord8 0x01
        putULEB128 $ LBS.length init
        putLazyByteString init
    get = do
        op <- getULEB128 32
        case (op :: Word8) of
            0x00 -> do
                offset <- getExpression
                len <- getULEB128 32
                init <- getLazyByteString len
                return $ DataSegment (ActiveData 0 offset) init
            0x01 -> do
                len <- getULEB128 32
                init <- getLazyByteString len
                return $ DataSegment PassiveData init 
            0x02 -> do
                memIdx <- getULEB128 32
                offset <- getExpression
                len <- getULEB128 32
                init <- getLazyByteString len
                return $ DataSegment (ActiveData memIdx offset) init
            byte -> fail $ "unknown data segment type: " ++ show byte 

instance Serialize Module where
    put mod = do
        -- magic
        mapM_ putWord8 [0x00, 0x61, 0x73, 0x6D]
        -- version
        mapM_ putWord8 [0x01, 0x00, 0x00, 0x00]

        putSection TypeSection $ putVec $ types mod
        putSection ImportSection $ putVec $ imports mod
        putSection FunctionSection $ putVec $ map (Index . funcType) $ functions mod
        putSection TableSection $ putVec $ tables mod
        putSection MemorySection $ putVec $ mems mod
        putSection GlobalSection $ putVec $ globals mod
        putSection ExportSection $ putVec $ exports mod
        case start mod of
            Just (StartFunction idx) -> putSection StartSection $ putULEB128 idx
            Nothing -> return ()
        putSection ElementSection $ putVec $ elems mod
        putSection CodeSection $ putVec $ functions mod
        putSection DataSection $ putVec $ datas mod
        
    get = do
        magic <- getWord32be
        if magic == 0x0061736D then return () else fail "magic header not detected"
        version <- getWord32be
        if version == 0x01000000 then return () else fail "unknown binary version"
        types <- getSection TypeSection getVec []
        imports <- getSection ImportSection getVec []
        funcTypes <- getSection FunctionSection getVec []
        tables <- getSection TableSection getVec []
        mems <- getSection MemorySection getVec []
        globals <- getSection GlobalSection getVec []
        exports <- getSection ExportSection getVec []
        start <- getSection StartSection (Just . StartFunction <$> getULEB128 32) Nothing
        elems <- getSection ElementSection getVec []
        functions <- getSection CodeSection getVec []
        datas <- getSection DataSection getVec []
        if length funcTypes /= length functions
        then fail "function and code section have inconsistent lengths"
        else return ()
        return $ emptyModule {
            types,
            imports,
            tables,
            mems,
            globals,
            exports,
            start,
            elems,
            functions = zipWith (\(Index funcType) fun -> fun { funcType }) funcTypes functions,
            datas
        }


dumpModule :: Module -> BS.ByteString
dumpModule = encode

dumpModuleLazy :: Module -> LBS.ByteString
dumpModuleLazy = encodeLazy

decodeModule :: BS.ByteString -> Either String Module
decodeModule = decode

decodeModuleLazy :: LBS.ByteString -> Either String Module
decodeModuleLazy = decodeLazy