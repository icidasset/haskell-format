module Format.Builder.Code where

import Data.Monoid ((<>))
import Flow

import Format.Parser.Comment (Comment(..))
import Format.Parser.Code (Code(..))

import qualified Data.List as List (map, intercalate)


-- 📮


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
