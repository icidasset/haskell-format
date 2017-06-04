module Format.Parser.Utilities where

import Control.Applicative (Alternative, (<|>))
import qualified Control.Applicative


-- ⚗️ Utilities


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
