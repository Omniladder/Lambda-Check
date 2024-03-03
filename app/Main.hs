{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

removeComments :: [T.Text] -> [T.Text]
removeComments input = filter (\x -> not (T.isInfixOf "--" x)) input

splitOnNewLine :: T.Text -> [T.Text]
splitOnNewLine input = T.splitOn "\n" input

splitOnComma :: T.Text -> [T.Text]
splitOnComma input = T.splitOn "," input

addWithNewLines :: [T.Text] -> [T.Text]
addWithNewLines input = map (`T.append` "\n") input

replaceWithComma :: T.Text -> T.Text
replaceWithComma input = T.replace "build-depends:" "build-depends:," (T.replace "extra-libraries:" "extra-libraries:," input)

trimEmpty :: [[T.Text]] -> [[T.Text]]
trimEmpty input = map( \x-> map T.strip x ) input

dropEmpty :: [[T.Text]] -> [[T.Text]]
dropEmpty input = map (\x -> dropWhile (=="")x) input

getHeads :: [[T.Text]] -> [T.Text]
getHeads input = map head input 


main :: IO ()
main = do
    fileContent <- TIO.readFile "../test.cabal"

    let split_output = splitOnNewLine fileContent

    let no_comments = removeComments split_output

    let add_new_lines = addWithNewLines no_comments

    let new_string = T.concat add_new_lines

    let with_commas = replaceWithComma new_string

    let comma_splits = splitOnComma with_commas

    let final_newline_split = map splitOnNewLine comma_splits

    let trimmed_empty = trimEmpty final_newline_split

    let empty_dropped = dropEmpty trimmed_empty

    let head_gotten = getHeads empty_dropped

    let drop_first = tail head_gotten



    print drop_first
    
