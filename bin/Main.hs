module Main where

import Data.Maybe (listToMaybe)
import Format
import System.Environment (getArgs)
import System.Exit


-- 🍯


main :: IO ()
main = do
    args <- getArgs

    -- Fun w/ flags
    let maybeFilePath = listToMaybe (excludeFlags args)

    -- Format!
    case maybeFilePath of
        Just filePath   -> formatFile args filePath
        Nothing         -> formatStdin args



-- 📮


{-| Format contents from a file.
-}
formatFile :: [String] -> String -> IO ()
formatFile _ filePath = do
    contents <- readFile filePath

    case Format.format contents filePath of
        Ok result   -> writeFile filePath result >> exitSuccess
        Err err     -> putStr err >> exitFailure


{-| Format contents from stdin.
-}
formatStdin :: [String] -> IO ()
formatStdin _ = do
    contents <- getContents

    case Format.format contents "" of
        Ok result   -> putStr result >> exitSuccess
        Err err     -> putStr err >> exitFailure



-- ⛳️


excludeFlags :: [String] -> [String]
excludeFlags = filter excludeFlag


excludeFlag :: String -> Bool
excludeFlag ('-' : _) = False
excludeFlag _ = True
