module Main where

import Format
import System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs

    -- Fun w/ flags
    let filePath = head args

    -- Format!
    Format.format filePath
