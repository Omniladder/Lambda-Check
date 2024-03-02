module Main where

main :: IO ()
main = do
    fileContent <- readFile "Lambda-Check.cabal"

    putStrLn fileContent
