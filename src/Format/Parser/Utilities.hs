module Format.Parser.Utilities where

import Control.Applicative (Alternative, (<|>))
import Flow
import Prelude hiding (or)
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Control.Applicative
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as List (splitOn)


-- ⚗️ Combinators


and :: (Applicative f, Monoid a) => f a -> f a -> f a
and =
    Control.Applicative.liftA2 mappend


andThen :: Monad m => m a -> m b -> m b
andThen =
    (>>)


or :: Alternative f => f a -> f a -> f a
or =
    (<|>)


one :: a -> a
one =
    id


maybeSome :: Alternative f => f a -> f [a]
maybeSome =
    Control.Applicative.many



-- 🤖 Predefined combinations


moduleName :: Parser String
moduleName =
    some (alphaNumChar `or` char '.' `or` char '_')


spaceCharacter :: Parser Char
spaceCharacter =
    char ' '


whitespace :: Parser Char
whitespace =
    spaceChar



-- 📿 Strings


leadingNewlines :: String -> Int
leadingNewlines str =
    str
        |> List.filter (== '\n')
        |> List.length


leadingSpace :: String -> Int
leadingSpace str =
    str
        |> List.splitOn "\n"
        |> List.last
        |> List.length


{-| Remove whitespace from the end of a string.

>>> trimEnd "Hello   "
"Hello"

>>> trimEnd "Hi \n\n"
"Hi"

>>> trimEnd "Foo \nbar"
"Foo \nbar"

-}
trimEnd :: String -> String
trimEnd =
    List.dropWhileEnd Char.isSpace
