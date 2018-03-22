{-

THE PARSER
==========

Parsing the Haskell files.

Wait what?
----------

Haskell file -> `Document` constructor.

Structure
---------

Document
    Module
    Imports
    Code

A `Document` represents a single Haskell file.

-}
module Format.Parser
    ( Code(..)
    , Comment(..)
    , Document(..)
    , Import(..)
    , ImportOptions(..)
    , Language(..)
    , Module(..)
    , Portable(..)
    , document
    ) where

import Format.Parser.Code
import Format.Parser.Comment
import Format.Parser.Import
import Format.Parser.Language
import Format.Parser.Module
import Format.Parser.Portable
import Format.Parser.Types
import Format.Parser.Utilities
import Text.Megaparsec (eof)


-- 🌳


data Document = Document [Language] Module [Import] [Code] deriving (Show)



-- 📮


document :: Parser Document
document = do
    _                   <- maybeSome whitespace
    languagePragmas     <- maybeSome language
    _                   <- maybeSome whitespace
    theModule           <- one docModule
    _                   <- maybeSome comment
    theImports          <- maybeSome docImport
    piecesOfCode        <- maybeSome code
    _                   <- eof

    return $ Document languagePragmas theModule theImports piecesOfCode
