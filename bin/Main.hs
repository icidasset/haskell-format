module Main where

import Format
import System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs

    let filePath = head args

    Format.format filePath
