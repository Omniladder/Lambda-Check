{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Network.HTTP.Conduit
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List
import System.Console.ANSI



removeQuotesAndSlashes :: T.Text -> T.Text
removeQuotesAndSlashes = T.replace "\"" "" . T.replace "\\" ""

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
trimEmpty input = map ( \x-> map T.strip x ) input

dropEmpty :: [[T.Text]] -> [[T.Text]]
dropEmpty input = map (\x -> dropWhile (=="") x) input

getHeads :: [[T.Text]] -> [T.Text]
getHeads input = map head input

-- createOutput :: [T.Text] -> [T.Text] -> [T.Text]
-- createOutput input1 input2 = map (\x y -> (x ++ y)) input1 input2


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

    let drop_first = nub (tail head_gotten)

    let route = "http://localhost:8080/search?term=" :: String

    let urls= map (\x->route ++ T.unpack (x)) drop_first

    responses <- mapM simpleHttp urls

    let converted_resposnes = map (TE.decodeUtf8 . LBS.toStrict) responses

    let trimmed_responses =map removeQuotesAndSlashes converted_resposnes

    let pairs = zip drop_first trimmed_responses
    setSGR [SetColor Background Dull Magenta]
    putStrLn " __         ______     __    __     ______     _____     ______"
    putStrLn "/\\ \\       /\\  __ \\   /\\ \ \ \\./  \\   /\\  == \\   /\\  __-.  /\\  __ \\"
    putStrLn "\\ \\ \\____  \\ \\  __ \\  \\ \\ \\-./\\ \\  \\ \\  __<   \\ \\ \\/\\ \\ \\ \\  __ \\"
    putStrLn " \\ \\_____\\  \\ \\_\\ \\_\\  \\ \\_\\ \\ \\_\\  \\ \\_____\\  \\ \\____-  \\ \\_\\ \\_\\"
    putStrLn "  \\/_____/   \\/_/\\/_/   \\/_/  \\/_/   \\/_____/   \\/____/   \\/_/\\/_/"
    putStrLn "                                                                    "
    putStrLn "                      ______     __  __     ______     ______     __  __    "
    putStrLn "                     /\\  ___\\   /\\ \\_\\ \\   /\\  ___\\   /\\  ___\\   /\\ \\/ /    "
    putStrLn "                     \\ \\ \\____  \\ \\  __ \\  \\ \\  __\\   \\ \\ \\____  \\ \\  _\"-.  "
    putStrLn "                      \\ \\_____\\  \\ \\_\\ \\_\\  \\ \\_____\\  \\ \\_____\\  \\ \\_\\ \\_\\ "
    putStrLn "                       \\/_____/   \\/_/\\/_/   \\/_____/   \\/_____/   \\/_/\\/_/ "

-- Ends the Highlighted Section
    putStrLn "\x1b[49m"
    putStrLn ""


    mapM_ (\x -> putStrLn(T.unpack(fst x) ++ " -- "++ T.unpack(snd x))) pairs