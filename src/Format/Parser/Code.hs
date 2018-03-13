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
    | QuasiQuote Int String String
    --
    -- Types
    | TypeAlias String String
    --
    -- Level 1
    -- # 1st argument = The leading spaces on the same line
    | Definition Int String
    | Specification Int String String
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
        , try quasiQuote

        -- Types
        , try typeAlias

        -- Level 1
        , try specification
        , try definition

        -- Fallback
        , unchartedLine
        ]


{-| A Quasiquotation.

>>> parseTest quasiQuote "[expr|abc|]"
QuasiQuote 0 "expr" "abc"

>>> parseTest quasiQuote "    [expr|\nabc\n|]"
QuasiQuote 4 "expr" "\nabc\n"

>>> parseTest quasiQuote "[expr|abc\ndef|]"
QuasiQuote 0 "expr" "abc\ndef"

>>> parseTest quasiQuote "  [expr|abc\ndef\n|]"
QuasiQuote 2 "expr" "abc\ndef\n"

-}
quasiQuote :: Parser Code
quasiQuote = do
    spaceBefore     <- maybeSome whitespace
    _               <- one (char '[')
    expression      <- some alphaNumChar
    _               <- one (char '|')
    quote           <- someTill anyChar (string "|]")
    _               <- optional eol

    return $ QuasiQuote (leadingSpace spaceBefore) expression quote



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
    _               <- optional spaceCharacter
    _               <- one (char '=')
    _               <- optional spaceCharacter
    for             <- someTill anyChar eol

    return $ TypeAlias name for



-- Level 1


{-| A specification.

>>> parseTest specification "specification :: (a -> b) -> Parser Code\n"
Specification 0 "specification" "(a -> b) -> Parser Code"

>>> parseTest specification "    spec :: c\n"
Specification 4 "spec" "c"

-}
specification :: Parser Code
specification = do
    spaceBefore     <- maybeSome whitespace
    functionName    <- some alphaNumChar
    _               <- optional spaceCharacter
    _               <- one (string "::")
    _               <- optional spaceCharacter
    functionType    <- someTill anyChar eol

    return $ Specification (leadingSpace spaceBefore) functionName functionType


{-| A definition.

>>> parseTest definition "definition = do"
Definition 0 "definition"

>>> parseTest definition "  def = "
Definition 2 "def"

-}
definition :: Parser Code
definition = do
    spaceBefore     <- maybeSome whitespace
    functionName    <- some alphaNumChar
    _               <- optional spaceCharacter
    _               <- one (char '=')

    return $ Definition (leadingSpace spaceBefore) functionName



-- Uncharted territory


unchartedLine :: Parser Code
unchartedLine = do
    line <- manyTill anyChar eol

    return $ UnchartedLine (trimEnd line)
