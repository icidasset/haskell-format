module Main where

import Format
import System.Environment (getArgs)
import System.Exit


main :: IO ()
main = do
    args <- getArgs

    -- Fun w/ flags
    let filePath = head args
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
