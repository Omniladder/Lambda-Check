module Main where
import Data.List.Split
import Data.List

removeComments :: [String] -> [String]
removeComments input = filter (\x -> not (isInfixOf "--" x)) input

splitOnNewLine :: String -> [String]
splitOnNewLine input = splitOn "\n" input

addWithNewLines :: [String] -> [String]
addWithNewLines input = map (++ "\n") input

main :: IO ()
main = do
    fileContent <- readFile "Lambda-Check.cabal"

    let split_output = splitOnNewLine fileContent

    let no_comments = removeComments split_output

    let add_new_lines = addWithNewLines no_comments

    let new_string = concat add_new_lines

    putStrLn new_string
    
