{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Network.HTTP.Conduit
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List
import System.Console.ANSI
import System.Directory (getDirectoryContents)
import System.FilePath ( takeExtension)

listCabalFiles :: FilePath -> IO [FilePath]
listCabalFiles dir = do
    contents <- getDirectoryContents dir
    return $ filter (\f -> takeExtension f == ".cabal") contents

listHsFiles :: FilePath -> IO [FilePath]
listHsFiles dir = do
    contents <- getDirectoryContents dir
    return $ filter (\f -> takeExtension f == ".hs") contents

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

removeVersions :: T.Text -> T.Text
removeVersions input = head (T.splitOn "^" (head (T.splitOn "=" (head (T.splitOn ">" (head (T.splitOn "<" input)))))))

cveAnalysis :: FilePath -> IO()
cveAnalysis filepath = do
        fileContent <- TIO.readFile filepath
        putStrLn ("\n Analyzing " ++ filepath) 

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

        let no_verisons = map removeVersions drop_first

        let route = "http://0.0.0.0:8000/search?term=" :: String

        let urls= map (\x->route ++ T.unpack (x)) no_verisons

        responses <- mapM simpleHttp urls

        let converted_resposnes = map (TE.decodeUtf8 . LBS.toStrict) responses

        let trimmed_responses =map removeQuotesAndSlashes converted_resposnes

        let pairs = zip no_verisons trimmed_responses
        mapM_ (\x -> putStrLn (" -- "++T.unpack (fst x) ++ " -- "++ T.unpack (snd x))) pairs

printLogo :: Int -> IO()
printLogo x= do
    setSGR [SetColor Background Dull Magenta]
    putStrLn " __         ______     __    __     ______     _____     ______                                              "
    putStrLn "/\\ \\       /\\  __ \\   /\\ \ \ \\./  \\   /\\  == \\   /\\  __-.  /\\  __ \\                         "
    putStrLn "\\ \\ \\____  \\ \\  __ \\  \\ \\ \\-./\\ \\  \\ \\  __<   \\ \\ \\/\\ \\ \\ \\  __ \\                       "
    putStrLn " \\ \\_____\\  \\ \\_\\ \\_\\  \\ \\_\\ \\ \\_\\  \\ \\_____\\  \\ \\____-  \\ \\_\\ \\_\\                      "
    putStrLn "  \\/_____/   \\/_/\\/_/   \\/_/  \\/_/   \\/_____/   \\/____/   \\/_/\\/_/                    "
    putStrLn "                                                                                               "
    putStrLn "                      ______     __  __     ______     ______     __  __                       "
    putStrLn "                     /\\  ___\\   /\\ \\_\\ \\   /\\  ___\\   /\\  ___\\   /\\ \\/ /           "
    putStrLn "                     \\ \\ \\____  \\ \\  __ \\  \\ \\  __\\   \\ \\ \\____  \\ \\  _\"-.      "
    putStrLn "                      \\ \\_____\\  \\ \\_\\ \\_\\  \\ \\_____\\  \\ \\_____\\  \\ \\_\\ \\_\\ "
    putStrLn "                       \\/_____/   \\/_/\\/_/   \\/_____/   \\/_____/   \\/_/\\/_/             "
-- Ends the Highlighted Section
    putStrLn "\x1b[49m"
    putStrLn ""

weaknessAnalysis :: FilePath -> IO()
weaknessAnalysis filePath =do
    fileContent <- TIO.readFile filePath
    setSGR [SetColor Foreground Dull White] >> putStrLn ("\n Analyzing " ++ filePath) 
    if T.isInfixOf "import Unsafe.Coerce" fileContent
        then 
            setSGR [SetColor Foreground Dull Red] >>
            putStrLn "-- Utilization of unsafeCoerce in type change operations can result in segmenation faults and data corruption"
        else 
            setSGR [SetColor Foreground Dull Green] >>
            putStrLn "-- No risk of unsafeCoerce segmentaion faults!"
    (if T.isInfixOf "peek" fileContent && T.isInfixOf "import Foreign.Ptr" fileContent 
        then 
            setSGR [SetColor Foreground Dull Red] >>
            putStrLn "-- Using peek on a foreign pointer can cause a segmentation fault, if null pointer segmentation fault is garaunteed" 
        else 
            setSGR [SetColor Foreground Dull Green] >>            
            putStrLn "-- No risk of derefrenceing null pointer with peek!")
    if T.isInfixOf "IORef" fileContent
        then 
            setSGR [SetColor Foreground Dull Red] >>
            putStrLn "-- Program is using mutable state via IORef which are vulnerable to buffer overflow"
        else 
            setSGR [SetColor Foreground Dull Green] >>
            putStrLn "-- No risk of buffer overflow from IORef"
    if T.isInfixOf "foreign import" fileContent
        then 
            setSGR [SetColor Foreground Dull Red] >>
            putStrLn "-- Foreign library import detected, non native libraties are more vulnerable to segmentaion faults and buffer overflows"
        else 
            setSGR [SetColor Foreground Dull Green] >>
            putStrLn "-- Foreign imports not found!"


main :: IO ()
main = do
    printLogo 1
    cabalFiles <- listCabalFiles "."
    mapM_ cveAnalysis cabalFiles
    hsFiles <- listHsFiles "."
    mapM_ weaknessAnalysis hsFiles

