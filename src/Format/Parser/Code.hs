module Format.Parser.Code where

import Format.Parser.Comment
import Format.Parser.Types
import Format.Parser.Utilities
import Prelude hiding (and, or)
import Text.Megaparsec
import Text.Megaparsec.Char


-- 🌳


data Code
    = Note Comment
    --
    -- Types
    | TypeAlias String String
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



-- Types


{-| A type alias.

>>> parseTest typeAlias "type Alias = (a -> b) -> Original\n"
TypeAlias "Alias" "(a -> b) -> Original"

-}
typeAlias :: Parser Code
typeAlias = do
    _               <- maybeSome whitespace
    _               <- one (string "type ")
    name            <- some alphaNumChar
    _               <- one (string " = ")
    for             <- someTill anyChar eol

    return $ TypeAlias name for



-- Level 1


{-| A specification.

>>> parseTest specification "specification :: (a -> b) -> Parser Code\n"
Specification "specification" "(a -> b) -> Parser Code"

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
