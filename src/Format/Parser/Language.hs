module Format.Parser.Language where

import Format.Parser.Types
import Format.Parser.Utilities
import Prelude hiding (or)
import Text.Megaparsec
import Text.Megaparsec.Char


-- 🌳


{-| `Language` type.

A `LANGUAGE` pragma containing a list of extensions.

-}
data Language = Language [String] deriving (Show)



-- 📮


{-| Language pragmas.
-}
language :: Parser Language
language = do
    _               <- maybeSome whitespace
    _               <- one (string "{-# LANGUAGE ")
    extensions      <- sepBy moduleName (string ", ")
    _               <- one (string " #-}")
    _               <- maybeSome whitespace

    return $ Language extensions
