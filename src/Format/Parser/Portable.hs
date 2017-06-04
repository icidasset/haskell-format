module Format.Parser.Portable where

import Flow
import Format.Parser.Utilities
import Prelude hiding (or)
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Functor.Identity as Functor (Identity)
import qualified Data.Maybe as Maybe


-- 🌳


{-| `Portable` type.

{!} A portable is either an export or an import.

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
    _               <- skipMany spaceChar
    thePortables    <- sepEndBy portable portablesSeparator
    _               <- char ')'

    return thePortables


portablesSeparator :: ParsecT Dec String Functor.Identity ()
portablesSeparator =
    maybeSome spaceChar `andThen` optional (char ',') `andThen` space


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
    constructors    <- optional (dataConstructors)

    constructors
        |> Maybe.fromMaybe []
        |> Portable name
        |> return


{-| A list of data constructors.

>>> parseTest dataConstructors "(A, B, C)"
["A","B","C"]

-}
dataConstructors :: Parser [String]
dataConstructors = do
    _               <- char '('
    constructors    <- sepBy dataConstructor (char ',' `andThen` space)
    _               <- char ')'

    return constructors


{-| A data constructor.
-}
dataConstructor :: Parser String
dataConstructor =
    some alphaNumChar `or` string ".."
