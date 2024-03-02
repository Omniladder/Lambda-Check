module Main where
import Data.List.Split
import Data.List


main :: IO ()
main = do
    fileContent <- readFile "Lambda-Check.cabal"
    let split_output = splitOn "\n" fileContent
    let no_comments = filter (\x -> not (isInfixOf "--" x)) split_output
    let filtered_output = dropWhile ( \x -> not (isInfixOf "build-depends:" x)) no_comments
    print filtered_output

    
