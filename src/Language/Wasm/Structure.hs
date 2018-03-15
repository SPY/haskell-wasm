{-# LANGUAGE DuplicateRecordFields #-}

module Language.Wasm.Structure (
    Module(..),
    DataSegment(..),
    ElemSegment(..),
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
    ResultType,
    Expression,
    LabelIndex,
    LocalIndex,
    GlobalIndex,
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

data BitSize = BS32 | BS64 deriving (Show, Eq)

data IUnOp = IClz | ICtz | IPopcnt deriving (Show, Eq)

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
    deriving (Show, Eq)

data IRelOp = IEq | INe | ILtU | ILtS | IGtU | IGtS | ILeU | ILeS | IGeU | IGeS deriving (Show, Eq)

data FUnOp = FAbs | FNeg | FCeil | FFloor | FTrunc | FNearest | FSqrt deriving (Show, Eq)

data FBinOp = FAdd | FSub | FMul | FDiv | FMin | FMax | FCopySign deriving (Show, Eq)

data FRelOp = FEq | FNe | FLt | FGt | FLe | FGe deriving (Show, Eq)

data MemArg = MemArg { align :: Natural, offset :: Natural } deriving (Show, Eq)

type LabelIndex = Natural
type FuncIndex = Natural
type TypeIndex = Natural
type LocalIndex = Natural
type GlobalIndex = Natural
type MemoryIndex = Natural
type TableIndex = Natural

data ValueType =
    I32
    | I64
    | F32
    | F64
    deriving (Show, Eq)

type ResultType = [ValueType]
type ParamsType = [ValueType]
type LocalsType = [ValueType]

data FuncType = FuncType { params :: ParamsType, results :: ResultType } deriving (Show, Eq)

data Instruction =
    -- Control instructions
    Unreachable
    | Nop
    | Block { result :: ResultType, body :: Expression }
    | Loop { result :: ResultType, body :: Expression }
    | If { result :: ResultType, true :: Expression, false :: Expression }
    | Br LabelIndex
    | BrIf LabelIndex
    | BrTable [LabelIndex] LabelIndex
    | Return
    | Call FuncIndex
    | CallIndirect TypeIndex
    -- Parametric instructions
    | Drop
    | Select
    -- Variable instructions
    | GetLocal LocalIndex
    | SetLocal LocalIndex
    | TeeLocal LocalIndex
    | GetGlobal GlobalIndex
    | SetGlobal GlobalIndex
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
    | CurrentMemory
    | GrowMemory
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
    | I64ExtendSI32
    | I64ExtendUI32
    | FConvertIU {- Float Size -} BitSize {- Int Size -} BitSize
    | FConvertIS {- Float Size -} BitSize {- Int Size -} BitSize
    | F32DemoteF64
    | F64PromoteF32
    | IReinterpretF BitSize
    | FReinterpretI BitSize
    deriving (Show, Eq)

type Expression = [Instruction]

data Function = Function {
    funcType :: TypeIndex,
    localTypes :: LocalsType,
    body :: Expression
} deriving (Show, Eq)

data Limit = Limit Natural (Maybe Natural) deriving (Show, Eq)

data ElemType = AnyFunc deriving (Show, Eq)

data TableType = TableType Limit ElemType deriving (Show, Eq)

data Table = Table TableType deriving (Show, Eq)

data Memory = Memory Limit deriving (Show, Eq)

data GlobalType = Const ValueType | Mut ValueType deriving (Show, Eq)

data Global = Global {
    globalType :: GlobalType,
    initializer :: Expression
} deriving (Show, Eq)

data ElemSegment = ElemSegment {
    tableIndex :: TableIndex,
    offset :: [Instruction],
    funcIndexes :: [FuncIndex]
} deriving (Show, Eq)

data DataSegment = DataSegment {
    memIndex :: MemoryIndex,
    offset :: Expression,
    chunk :: LBS.ByteString
} deriving (Show, Eq)

data StartFunction = StartFunction FuncIndex deriving (Show, Eq)

data ExportDesc =
    ExportFunc FuncIndex
    | ExportTable TableIndex
    | ExportMemory MemoryIndex
    | ExportGlobal GlobalIndex
    deriving (Show, Eq)

data Export = Export {
    name :: TL.Text,
    desc :: ExportDesc
} deriving (Show, Eq)

data ImportDesc =
    ImportFunc TypeIndex
    | ImportTable TableType
    | ImportMemory Limit
    | ImportGlobal GlobalType
    deriving (Show, Eq)

data Import = Import {
    sourceModule :: TL.Text,
    name :: TL.Text,
    desc :: ImportDesc
} deriving (Show, Eq)

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
} deriving (Show, Eq)

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
