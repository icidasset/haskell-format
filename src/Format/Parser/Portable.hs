{-| A portable is either an export or an import.
-}
module Format.Parser.Portable where

import Format.Parser.Utilities
import Prelude hiding (and, or)
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
    _               <- maybeSome whitespace
    thePortables    <- sepEndBy portable portableSeparator
    _               <- char ')'

    return thePortables


portableSeparator :: Parser String
portableSeparator =
    maybeSome whitespace `andThen` optional (char ',') `andThen` maybeSome whitespace


{-| A portable.

>>> parseTest portable "Alias"
Portable "Alias" []

>>> parseTest portable "Type(..)"
Portable "Type" [".."]

>>> parseTest portable "Type(A, B)"
Portable "Type" ["A","B"]

>>> parseTest portable "function"
Portable "function" []

>>> parseTest portable "module Abc"
Portable "module Abc" []

>>> parseTest portable "(!~>)"
Portable "(!~>)" []

-}
portable :: Parser Portable
portable = do
    name            <- one portableName
    constructors    <- optional dataConstructors

    return $
        Portable
            name
            (Maybe.fromMaybe [] constructors)


portableName :: Parser String
portableName = choice
    [ string "module" `and` some spaceCharacter `and` one moduleName
    , one infixPortableName
    , one moduleName
    ]


infixPortableName :: Parser String
infixPortableName = do
    opening         <- string "("
    exceptTheEnd    <- someTill (noneOf ")") (char ')')

    return $ concat [opening, exceptTheEnd, ")"]


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


dataConstructorSeparator :: Parser String
dataConstructorSeparator =
    char ',' `andThen` maybeSome spaceCharacter


{-| A data constructor.

>>> parseTest dataConstructor "Format"
"Format"

>>> parseTest dataConstructor ".."
".."

-}
dataConstructor :: Parser String
dataConstructor =
    some alphaNumChar `or` string ".."
