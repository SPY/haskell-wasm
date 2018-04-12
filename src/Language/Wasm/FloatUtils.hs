{-# LANGUAGE FlexibleContexts #-}

module Language.Wasm.FloatUtils (
    wordToFloat,
    floatToWord,
    wordToDouble,
    doubleToWord,
    makeNaN,
    doubleToFloat
) where

import Data.Word (Word32, Word64)
import Data.Bits ((.|.), (.&.), shiftR)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

-- brough from https://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

makeNaN :: Word64 -> Double
makeNaN w = wordToDouble $ 0x7FF0000000000000 .|. (0x000FFFFFFFFFFFFF .&. w)

doubleToFloat :: Double -> Float
doubleToFloat d =
    let w = doubleToWord d in
    if 0x7FF0000000000000 == (w .&. 0x7FF0000000000000) && (w .&. 0x0007FFFFFFFFFFFF) /= 0
    then wordToFloat $ fromIntegral $ ((0x8000000000000000 .&. w) `shiftR` 32) .|. 0x7F800000 .|. (0x7FFFFF .&. w)
    else realToFrac d