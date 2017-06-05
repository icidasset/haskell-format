module Format.Builder.Portable where

import Data.Monoid ((<>))
import Format.Parser.Portable (Portable(..))

import qualified Data.List as List (intercalate)


-- 📮


build :: Portable -> String
build (Portable name dataConstructors) = concat
    [ -- Name
      name

      -- Data constructors
    , case dataConstructors of
        [] ->
            ""

        list ->
            "(" <> List.intercalate ", " list <> ")"
    ]
