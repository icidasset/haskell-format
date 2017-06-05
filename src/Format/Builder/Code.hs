module Format.Builder.Code where

import Data.Monoid ((<>))
import Flow
import Format.Parser

import qualified Data.List as List (map, intercalate)


-- 📮


{-| Build a list of `Code`s.
-}
buildList :: [Code] -> String
buildList pieces =
    pieces
        |> List.map
            (\code ->
                case code of
                    Note (Comment comment) ->
                        "-- " <> comment

                    Note (CommentBlock comment) ->
                        "{-" <> comment <> "-}"

                    UnchartedLine line ->
                        line
            )
        |> List.intercalate "\n"
