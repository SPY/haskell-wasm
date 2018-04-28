module Main where

data OutputMode = WasmBinary | JSWrapper deriving (Show, Eq)

data WasmConfig = WasmConfig {
    inputFile :: String,
    outputFile :: String,
    outputMode :: OutputMode
} deriving (Show, Eq)

main :: IO ()
main = do
    putStrLn "There is no WASM!"
