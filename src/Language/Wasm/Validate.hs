{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Wasm.Validate (
    ValidationResult(..),
    validate,
    isValid
) where

import Language.Wasm.Structure
import qualified Data.Set as Set
import Data.List (foldl')
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

data ValidationResult =
    DuplicatedExportNames [String]
    | InvalidTableType
    | MoreThanOneMemory
    | MoreThanOneTable
    | Valid
    deriving (Show, Eq)

instance Monoid ValidationResult where
    mempty = Valid
    mappend Valid vr = vr
    mappend vr Valid = vr
    mappend vr _ = vr

isValid :: ValidationResult -> Bool
isValid Valid = True
isValid _ = False

type Validator = Module -> ValidationResult

data VType =
    Val ValueType
    | Var
    | LabelRef LabelIndex
    | LocalRef LocalIndex
    | GlobalRef GlobalIndex
    | Any
    | Result
    deriving (Show, Eq)

type End = [VType]

empty :: [ValueType]
empty = []

class ToEnd a where
    toEnd :: a -> [VType]

instance ToEnd VType where
    toEnd val = [val]

instance ToEnd ValueType where
    toEnd val = [Val val]

instance ToEnd [ValueType] where
    toEnd = map Val

instance ToEnd [VType] where
    toEnd = id

data Arrow = Arrow End End deriving (Show, Eq)

(==>) :: (ToEnd a, ToEnd b) => a -> b -> Arrow
(==>) a b = Arrow (toEnd a) (toEnd b)

getInstrType :: Instruction -> Arrow
getInstrType Unreachable = Any ==> Any
getInstrType Nop = empty ==> empty
getInstrType Block { result } = empty ==> result
getInstrType Loop { result } = empty ==> result
getInstrType If { result } = I32 ==> result
getInstrType (Br lbl) = [Any, LabelRef lbl] ==> Any
getInstrType (BrIf lbl) = [LabelRef lbl, Val I32] ==> LabelRef lbl
getInstrType (BrTable _ lbl) = [Any, LabelRef lbl, Val I32] ==> Any
getInstrType Return = [Any, Result] ==> Any
getInstrType (Call _) = Any ==> Any
getInstrType (CallIndirect _) = [Any, Val I32] ==> Any
getInstrType Drop = Var ==> empty
getInstrType Select = [Var, Var, Val I32] ==> Var
getInstrType (GetLocal local) = empty ==> LocalRef local
getInstrType (SetLocal local) = LocalRef local ==> empty
getInstrType (TeeLocal local) = LocalRef local ==> LocalRef local
getInstrType (GetGlobal global) = empty ==> GlobalRef global
getInstrType (SetGlobal global) = GlobalRef global ==> empty
getInstrType (I32Load _) = I32 ==> I32
getInstrType (I64Load _) = I32 ==> I64
getInstrType (F32Load _) = I32 ==> F32
getInstrType (F64Load _) = I32 ==> F64
getInstrType (I32Load8S _) = I32 ==> I32
getInstrType (I32Load8U _) = I32 ==> I32
getInstrType (I32Load16S _) = I32 ==> I32
getInstrType (I32Load16U _) = I32 ==> I32
getInstrType (I64Load8S _) = I32 ==> I64
getInstrType (I64Load8U _) = I32 ==> I64
getInstrType (I64Load16S _) = I32 ==> I64
getInstrType (I64Load16U _) = I32 ==> I64
getInstrType (I64Load32S _) = I32 ==> I64
getInstrType (I64Load32U _) = I32 ==> I64
getInstrType (I32Store _) = [I32, I32] ==> empty
getInstrType (I64Store _) = [I32, I64] ==> empty
getInstrType (F32Store _) = [I32, F32] ==> empty
getInstrType (F64Store _) = [I32, F64] ==> empty
getInstrType (I32Store8 _) = [I32, I32] ==> empty
getInstrType (I32Store16 _) = [I32, I32] ==> empty
getInstrType (I64Store8 _) = [I32, I64] ==> empty
getInstrType (I64Store16 _) = [I32, I64] ==> empty
getInstrType (I64Store32 _) = [I32, I64] ==> empty
getInstrType CurrentMemory = empty ==> I32
getInstrType GrowMemory = I32 ==> I32
getInstrType (I32Const _) = empty ==> I32
getInstrType (I64Const _) = empty ==> I64
getInstrType (F32Const _) = empty ==> F32
getInstrType (F64Const _) = empty ==> F64
getInstrType (IUnOp BS32 _) = I32 ==> I32
getInstrType (IUnOp BS64 _) = I64 ==> I64
getInstrType (IBinOp BS32 _) = [I32, I32] ==> I32
getInstrType (IBinOp BS64 _) = [I64, I64] ==> I64
getInstrType I32Eqz = I32 ==> I32
getInstrType I64Eqz = I64 ==> I32
getInstrType (IRelOp BS32 _) = [I32, I32] ==> I32
getInstrType (IRelOp BS64 _) = [I64, I64] ==> I32
getInstrType (FUnOp BS32 _) = F32 ==> F32
getInstrType (FUnOp BS64 _) = F64 ==> F64
getInstrType (FBinOp BS32 _) = [F32, F32] ==> F32
getInstrType (FBinOp BS64 _) = [F64, F64] ==> F64
getInstrType (FRelOp BS32 _) = [F32, F32] ==> I32
getInstrType (FRelOp BS64 _) = [F64, F64] ==> I32
getInstrType I32WrapI64 = I64 ==> I32
getInstrType (ITruncFU BS32 BS32) = F32 ==> I32
getInstrType (ITruncFU BS32 BS64) = F64 ==> I32
getInstrType (ITruncFU BS64 BS32) = F32 ==> I64
getInstrType (ITruncFU BS64 BS64) = F64 ==> I64
getInstrType (ITruncFS BS32 BS32) = F32 ==> I32
getInstrType (ITruncFS BS32 BS64) = F64 ==> I32
getInstrType (ITruncFS BS64 BS32) = F32 ==> I64
getInstrType (ITruncFS BS64 BS64) = F64 ==> I64
getInstrType I64ExtendSI32 = I32 ==> I64
getInstrType I64ExtendUI32 = I32 ==> I64
getInstrType (FConvertIU BS32 BS32) = I32 ==> F32
getInstrType (FConvertIU BS32 BS64) = I64 ==> F32
getInstrType (FConvertIU BS64 BS32) = I32 ==> F64
getInstrType (FConvertIU BS64 BS64) = I64 ==> F64
getInstrType (FConvertIS BS32 BS32) = I32 ==> F32
getInstrType (FConvertIS BS32 BS64) = I64 ==> F32
getInstrType (FConvertIS BS64 BS32) = I32 ==> F64
getInstrType (FConvertIS BS64 BS64) = I64 ==> F64
getInstrType F32DemoteF64 = F64 ==> F32
getInstrType F64PromoteF32 = F32 ==> F64
getInstrType (IReinterpretF BS32) = F32 ==> I32
getInstrType (IReinterpretF BS64) = F64 ==> I64
getInstrType (FReinterpretI BS32) = I32 ==> F32
getInstrType (FReinterpretI BS64) = I64 ==> F64

tablesShouldBeValid :: Validator
tablesShouldBeValid Module { imports, tables } =
    let tableImports = filter isTableImport imports in
    let res = foldMap (\Import { desc = ImportTable t } -> isValidTableType t) tableImports in
    let res' = foldl' (\r (Table t) -> r <> isValidTableType t) res tables in
    if length tableImports + length tables <= 1
        then res'
        else MoreThanOneTable
    where
        isValidTableType :: TableType -> ValidationResult
        isValidTableType (TableType (Limit min max) _) = if min <= fromMaybe min max then Valid else InvalidTableType

        isTableImport Import { desc = ImportTable _ } = True
        isTableImport _ = False

shouldBeAtMostOneMemory :: Validator
shouldBeAtMostOneMemory Module { imports, mems } =
    let memImports = filter isMemImport imports in
    if length memImports + length mems <= 1
    then Valid
    else MoreThanOneMemory
    where
        isMemImport Import { desc = ImportMemory _ } = True
        isMemImport _ = False

exportNamesShouldBeDifferent :: Validator
exportNamesShouldBeDifferent Module { exports } =
    case foldl' go (Set.empty, []) exports of
        (_, []) -> Valid
        (_, dup) -> DuplicatedExportNames dup
    where
        go :: (Set.Set TL.Text, [String]) -> Export -> (Set.Set TL.Text, [String])
        go (set, dup) (Export name _) =
            if Set.member name set
            then (set, show name : dup)
            else (Set.insert name set, dup)

validate :: Validator
validate mod = foldMap ($ mod) validators
    where
        validators :: [Validator]
        validators = [
                tablesShouldBeValid,
                shouldBeAtMostOneMemory,
                exportNamesShouldBeDifferent
            ]
