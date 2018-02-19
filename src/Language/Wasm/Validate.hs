module Language.Wasm.Validate (
    validate
) where

import Language.Wasm.Structure

validate :: Module -> Either String Module
validate mod = Right mod