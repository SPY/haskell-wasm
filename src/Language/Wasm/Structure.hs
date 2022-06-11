{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Wasm.Structure (
    Module(..),
    DataMode(..),
    DataSegment(..),
    ElemSegment(..),
    ElemMode(..),
    StartFunction(..),
    Export(..),
    ExportDesc(..),
    Table(..),
    Memory(..),
    Global(..),
    Function(..),
    Import(..),
    ImportDesc(..),
    Instruction(..),
    MemArg(..),
    IUnOp(..),
    IBinOp(..),
    IRelOp(..),
    FUnOp(..),
    FBinOp(..),
    FRelOp(..),
    BitSize(..),
    TableType(..),
    ElemType(..),
    Limit(..),
    GlobalType(..),
    FuncType(..),
    ValueType(..),
    BlockType(..),
    ParamsType,
    ResultType,
    LocalsType,
    Expression,
    LabelIndex,
    FuncIndex,
    TypeIndex,
    LocalIndex,
    GlobalIndex,
    MemoryIndex,
    TableIndex,
    emptyModule,
    isFuncImport,
    isTableImport,
    isMemImport,
    isGlobalImport
) where

import Numeric.Natural (Natural)
import Data.Word (Word32, Word64)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as TL
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data BitSize = BS32 | BS64 deriving (Show, Eq, Generic, NFData)

data IUnOp =
    IClz
    | ICtz
    | IPopcnt
    | IExtend8S
    | IExtend16S
    | IExtend32S
    deriving (Show, Eq, Generic, NFData)

data IBinOp =
    IAdd
    | ISub
    | IMul
    | IDivU
    | IDivS
    | IRemU
    | IRemS
    | IAnd
    | IOr
    | IXor
    | IShl
    | IShrU
    | IShrS
    | IRotl
    | IRotr
    deriving (Show, Eq, Generic, NFData)

data IRelOp = IEq | INe | ILtU | ILtS | IGtU | IGtS | ILeU | ILeS | IGeU | IGeS deriving (Show, Eq, Generic, NFData)

data FUnOp = FAbs | FNeg | FCeil | FFloor | FTrunc | FNearest | FSqrt deriving (Show, Eq, Generic, NFData)

data FBinOp = FAdd | FSub | FMul | FDiv | FMin | FMax | FCopySign deriving (Show, Eq, Generic, NFData)

data FRelOp = FEq | FNe | FLt | FGt | FLe | FGe deriving (Show, Eq, Generic, NFData)

data MemArg = MemArg { offset :: Natural, align :: Natural } deriving (Show, Eq, Generic, NFData)

type LabelIndex = Natural
type FuncIndex = Natural
type TypeIndex = Natural
type LocalIndex = Natural
type GlobalIndex = Natural
type MemoryIndex = Natural
type TableIndex = Natural
type DataIndex = Natural
type ElemIndex = Natural

data ValueType =
    I32
    | I64
    | F32
    | F64
    | Func
    | Extern
    deriving (Show, Eq, Generic, NFData)

type ResultType = [ValueType]
type ParamsType = [ValueType]
type LocalsType = [ValueType]

data FuncType = FuncType { params :: ParamsType, results :: ResultType } deriving (Show, Eq, Generic, NFData)

data BlockType =
    Inline (Maybe ValueType)
    | TypeIndex TypeIndex
    deriving (Show, Eq, Generic, NFData)

data Instruction index =
    -- Control instructions
    Unreachable
    | Nop
    | Block { blockType :: BlockType, body :: Expression }
    | Loop { blockType :: BlockType, body :: Expression }
    | If { blockType :: BlockType, true :: Expression, false :: Expression }
    | Br index
    | BrIf index
    | BrTable [index] index
    | Return
    | Call index
    | CallIndirect index index
    -- Reference instructions
    | RefNull ElemType
    | RefIsNull
    | RefFunc index
    | RefExtern Natural
    -- Parametric instructions
    | Drop
    | Select (Maybe [ValueType])
    -- Variable instructions
    | GetLocal index
    | SetLocal index
    | TeeLocal index
    | GetGlobal index
    | SetGlobal index
    -- Memory instructions
    | I32Load MemArg
    | I64Load MemArg
    | F32Load MemArg
    | F64Load MemArg
    | I32Load8S MemArg
    | I32Load8U MemArg
    | I32Load16S MemArg
    | I32Load16U MemArg
    | I64Load8S MemArg
    | I64Load8U MemArg
    | I64Load16S MemArg
    | I64Load16U MemArg
    | I64Load32S MemArg
    | I64Load32U MemArg
    | I32Store MemArg
    | I64Store MemArg
    | F32Store MemArg
    | F64Store MemArg
    | I32Store8 MemArg
    | I32Store16 MemArg
    | I64Store8 MemArg
    | I64Store16 MemArg
    | I64Store32 MemArg
    | MemorySize
    | MemoryGrow
    | MemoryFill
    | MemoryCopy
    | MemoryInit DataIndex
    | DataDrop DataIndex
    -- Table instructions
    | TableInit TableIndex ElemIndex
    | TableGrow TableIndex
    | TableSize TableIndex
    | TableFill TableIndex
    | TableGet TableIndex
    | TableSet TableIndex
    | TableCopy TableIndex TableIndex
    | ElemDrop ElemIndex
    -- Numeric instructions
    | I32Const Word32
    | I64Const Word64
    | F32Const Float
    | F64Const Double
    | IUnOp BitSize IUnOp
    | IBinOp BitSize IBinOp
    | I32Eqz
    | I64Eqz
    | IRelOp BitSize IRelOp
    | FUnOp BitSize FUnOp
    | FBinOp BitSize FBinOp
    | FRelOp BitSize FRelOp
    | I32WrapI64
    | ITruncFU {- Int Size -} BitSize {- Float Size -} BitSize
    | ITruncFS {- Int Size -} BitSize {- Float Size -} BitSize
    | ITruncSatFU {- Int Size -} BitSize {- Float Size -} BitSize
    | ITruncSatFS {- Int Size -} BitSize {- Float Size -} BitSize
    | I64ExtendSI32
    | I64ExtendUI32
    | FConvertIU {- Float Size -} BitSize {- Int Size -} BitSize
    | FConvertIS {- Float Size -} BitSize {- Int Size -} BitSize
    | F32DemoteF64
    | F64PromoteF32
    | IReinterpretF BitSize
    | FReinterpretI BitSize
    deriving (Show, Eq, Generic, NFData)

type Expression = [Instruction Natural]

data Function = Function {
    funcType :: TypeIndex,
    localTypes :: LocalsType,
    body :: Expression
} deriving (Show, Eq, Generic, NFData)

data Limit = Limit Natural (Maybe Natural) deriving (Show, Eq, Generic, NFData)

data ElemType = FuncRef | ExternRef deriving (Show, Eq, Generic, NFData)

data TableType = TableType Limit ElemType deriving (Show, Eq, Generic, NFData)

data Table = Table TableType deriving (Show, Eq, Generic, NFData)

data Memory = Memory Limit deriving (Show, Eq, Generic, NFData)

data GlobalType = Const ValueType | Mut ValueType deriving (Show, Eq, Generic, NFData)

data Global = Global {
    globalType :: GlobalType,
    initializer :: Expression
} deriving (Show, Eq, Generic, NFData)

data ElemMode =
    Passive
    | Active TableIndex Expression
    | Declarative
    deriving (Show, Eq, Generic, NFData)

data ElemSegment = ElemSegment {
    elemType :: ElemType,
    mode :: ElemMode,
    elements :: [Expression]
} deriving (Show, Eq, Generic, NFData)

data DataMode =
    PassiveData
    | ActiveData MemoryIndex Expression
    deriving (Show, Eq, Generic, NFData)

data DataSegment = DataSegment {
    dataMode :: DataMode,
    chunk :: LBS.ByteString
} deriving (Show, Eq, Generic, NFData)

data StartFunction = StartFunction FuncIndex deriving (Show, Eq, Generic, NFData)

data ExportDesc =
    ExportFunc FuncIndex
    | ExportTable TableIndex
    | ExportMemory MemoryIndex
    | ExportGlobal GlobalIndex
    deriving (Show, Eq, Generic, NFData)

data Export = Export {
    name :: TL.Text,
    desc :: ExportDesc
} deriving (Show, Eq, Generic, NFData)

data ImportDesc =
    ImportFunc TypeIndex
    | ImportTable TableType
    | ImportMemory Limit
    | ImportGlobal GlobalType
    deriving (Show, Eq, Generic, NFData)

data Import = Import {
    sourceModule :: TL.Text,
    name :: TL.Text,
    desc :: ImportDesc
} deriving (Show, Eq, Generic, NFData)

isFuncImport :: Import -> Bool
isFuncImport (Import _ _ (ImportFunc _)) = True
isFuncImport _ = False

isTableImport :: Import -> Bool
isTableImport (Import _ _ (ImportTable _)) = True
isTableImport _ = False

isMemImport :: Import -> Bool
isMemImport (Import _ _ (ImportMemory _)) = True
isMemImport _ = False

isGlobalImport :: Import -> Bool
isGlobalImport (Import _ _ (ImportGlobal _)) = True
isGlobalImport _ = False

data Module = Module {
    types :: [FuncType],
    functions :: [Function],
    tables :: [Table],
    mems :: [Memory],
    globals :: [Global],
    elems :: [ElemSegment],
    datas :: [DataSegment],
    start :: Maybe StartFunction,
    imports :: [Import],
    exports :: [Export]
} deriving (Show, Eq, Generic, NFData)

emptyModule :: Module
emptyModule = Module {
    types = [],
    functions = [],
    tables = [],
    mems = [],
    globals = [],
    elems = [],
    datas = [],
    start = Nothing,
    imports = [],
    exports = []
}
