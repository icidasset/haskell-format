module Format.Parser
    ( Document(..)
    , Import(..)
    , Module(..)
    , Portable(..)

    , document
    ) where

{-

Document
    Module
    Imports
    Code

A `Document` represents a single Haskell file.

-}

import Format.Parser.Module
import Format.Parser.Import
import Format.Parser.Portable
import Format.Parser.Utilities
import Text.Megaparsec
import Text.Megaparsec.String


-- 🌳


data Document = Document Module [Import] deriving (Show)



-- 📮


document :: Parser Document
document = do
    theModule       <- one docModule
    theImports      <- maybeSome docImport

    return $ Document theModule theImports
