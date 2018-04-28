module Main where

data WasmConfig = WasmConfig {
    inputFile :: String,
    outputFile :: String
} deriving (Show, Eq)

main :: IO ()
main = do
    putStrLn "There is no WASM!"
