module Format.Builder.Language where

import Data.Monoid ((<>))
import Flow
import Format.Parser

import qualified Data.List as List (intercalate, map)


-- 📮


{-| Build a `Language` list.
-}
buildList :: [Language] -> String
buildList list =
    list
        |> List.map build
        |> List.intercalate "\n"


{-| Build a `Language`.
-}
build :: Language -> String
build (Language extensions) =
    ""
    <> "{-# LANGUAGE "
    <> (List.intercalate ", " extensions)
    <> " #-}"
