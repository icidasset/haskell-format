module Format.Parser.Utilities where

import Control.Applicative (Alternative, (<|>))
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Control.Applicative


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


whitespace :: Parser ()
whitespace =
    skipMany spaceChar


spaceCharacters :: Parser ()
spaceCharacters =
    skipMany (char ' ')
