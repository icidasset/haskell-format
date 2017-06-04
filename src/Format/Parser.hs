module Format.Parser
    ( Comment(..)
    , Document(..)
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

import Format.Parser.Comment
import Format.Parser.Module
import Format.Parser.Import
import Format.Parser.Portable
import Format.Parser.Utilities
import Prelude hiding (or)
import Text.Megaparsec
import Text.Megaparsec.String


-- 🌳


data Document = Document Module [Import] deriving (Show)



-- 📮


document :: Parser Document
document = do
    theModule           <- one docModule
    _                   <- maybeSome comment
    theImports          <- maybeSome docImport

    return $ Document theModule theImports
