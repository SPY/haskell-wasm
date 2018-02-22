{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Wasm.Validate (
    validate,
    isValid
) where

import Language.Wasm.Structure
import qualified Data.Set as Set
import Data.List (foldl')
import qualified Data.Text.Lazy as TL

data ValidationResult =
    DuplicatedExportNames [String]
    | MoreThanOneMemory
    | MoreThanOneTable
    | Valid
    deriving (Show, Eq)

isValid :: ValidationResult -> Bool
isValid Valid = True
isValid _ = False

type Validator = Module -> ValidationResult

shouldBeAtMostOneTable :: Validator
shouldBeAtMostOneTable Module { imports, tables } =
    let memImports = filter isTableImport imports in
    if length memImports + length tables <= 1
    then Valid
    else MoreThanOneTable
    where
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
validate mod = foldl' go Valid validators
    where
        go :: ValidationResult -> Validator -> ValidationResult
        go Valid validator = validator mod
        go res _ = res

        validators :: [Validator]
        validators = [
                shouldBeAtMostOneTable,
                shouldBeAtMostOneMemory,
                exportNamesShouldBeDifferent
            ]
