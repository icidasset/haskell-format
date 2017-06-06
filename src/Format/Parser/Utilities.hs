module Format.Parser.Utilities where

import Control.Applicative (Alternative, (<|>))
import Flow
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Control.Applicative
import qualified Data.List as List
import qualified Data.List.Split as List (splitOn)


-- ⚗️ Combinators & stuff


and :: Monad m => m a -> (a -> m b) -> m b
and =
    (>>=)


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


whitespace :: Parser String
whitespace =
    maybeSome spaceChar


spaceCharacters :: Parser String
spaceCharacters =
    maybeSome (char ' ')



-- 📿 Strings


leadingSpace :: String -> Int
leadingSpace str =
    str
    |> List.splitOn "\n"
    |> List.last
    |> List.length
