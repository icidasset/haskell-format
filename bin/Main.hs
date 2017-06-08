module Main where

import Format
import System.Environment (getArgs)
import System.Exit


main :: IO ()
main = do
    args <- getArgs

    -- Fun w/ flags
    let filePath = head (excludeFlags args)
    let override = elem "--replace" args || elem "-r" args

    -- Get file contents
    contents <- readFile filePath

    -- Format!
    case Format.format contents filePath of
        Ok result ->
            if override then
                writeFile filePath result >> exitSuccess
            else
                putStr result >> exitSuccess

        Err err ->
            putStr err >> exitFailure



-- More fun with flags


excludeFlags :: [String] -> [String]
excludeFlags = filter excludeFlag


excludeFlag :: String -> Bool
excludeFlag ('-' : _) = False
excludeFlag _ = True
