{-| A portable is either an export or an import.
-}
module Format.Parser.Portable where

import Format.Parser.Utilities
import Prelude hiding (or)
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Maybe as Maybe


-- 🌳


{-| `Portable` type.

1st argument = The name of the portable
2nd argument = A list of data constructors

-}
data Portable =
    Portable String [String] deriving (Show)



-- 📮


{-| Portables.

    Many `portable`s between parentheses,
    separated by a comma and optional whitespace.

>>> parseTest portables "(a, b)"
[Portable "a" [],Portable "b" []]

>>> parseTest portables "( a , b )"
[Portable "a" [],Portable "b" []]

-}
portables :: Parser [Portable]
portables = do
    _               <- char '('
    _               <- whitespace
    thePortables    <- sepEndBy portable portableSeparator
    _               <- char ')'

    return thePortables


portableSeparator :: Parser ()
portableSeparator =
    whitespace `andThen` optional (char ',') `andThen` whitespace


{-| A portable.

>>> parseTest portable "Alias"
Portable "Alias" []

>>> parseTest portable "Type(..)"
Portable "Type" [".."]

>>> parseTest portable "Type(A, B)"
Portable "Type" ["A","B"]

>>> parseTest portable "function"
Portable "function" []

-}
portable :: Parser Portable
portable = do
    name            <- some alphaNumChar
    constructors    <- optional dataConstructors

    return $
        Portable
            name
            (Maybe.fromMaybe [] constructors)


{-| A list of data constructors.

>>> parseTest dataConstructors "(A, B, C)"
["A","B","C"]

-}
dataConstructors :: Parser [String]
dataConstructors = do
    _               <- char '('
    constructors    <- sepBy dataConstructor dataConstructorSeparator
    _               <- char ')'

    return constructors


dataConstructorSeparator :: Parser ()
dataConstructorSeparator =
    char ',' `andThen` spaceCharacters


{-| A data constructor.
-}
dataConstructor :: Parser String
dataConstructor =
    some alphaNumChar `or` string ".."
