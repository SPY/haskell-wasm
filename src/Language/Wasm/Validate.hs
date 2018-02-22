{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
