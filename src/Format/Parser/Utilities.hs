module Format.Parser.Utilities where

import Control.Applicative (Alternative, (<|>))
import Flow
import Prelude hiding (or)
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Control.Applicative
import qualified Data.List as List
import qualified Data.List.Split as List (splitOn)


-- ⚗️ Combinators & stuff


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
    some (alphaNumChar `or` char '.')


whitespace :: Parser Char
whitespace =
    spaceChar


spaceCharacter :: Parser Char
spaceCharacter =
    char ' '



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


trim :: String -> String
trim =
    words .> unwords
