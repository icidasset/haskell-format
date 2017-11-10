module Format.Parser.Code where

import Format.Parser.Comment
import Format.Parser.Types
import Format.Parser.Utilities
import Prelude hiding (or)
import Text.Megaparsec
import Text.Megaparsec.Char


-- 🌳


data Code
    = Note Comment
    --
    -- Level 1
    | Definition String
    | Specification String String
    --
    -- Uncharted territory
    -- > ie. an unparsed line of code
    | UnchartedLine String
    deriving (Show)



-- 📮


code :: Parser Code
code =
    choice
        [ try (fmap Note comment)
        , try specification
        , try definition

        -- Fallback
        , unchartedLine
        ]



-- Level 1


{-| A specification.

>>> parseTest specification "specification :: Parser (a -> b) -> Code\n"
Specification "specification" "Parser (a -> b) -> Code"

-}
specification :: Parser Code
specification = do
    _               <- maybeSome whitespace
    functionName    <- some alphaNumChar
    _               <- one (string " :: ")
    functionType    <- someTill anyChar eol

    return $ Specification functionName functionType


{-| A definition.

>>> parseTest definition "definition = do"
Definition "definition"

-}
definition :: Parser Code
definition = do
    _               <- maybeSome whitespace
    functionName    <- some alphaNumChar
    _               <- one (string " = ")

    return $ Definition functionName



-- Uncharted territory


unchartedLine :: Parser Code
unchartedLine = do
    line <- manyTill anyChar eol

    return $ UnchartedLine (trimEnd line)
