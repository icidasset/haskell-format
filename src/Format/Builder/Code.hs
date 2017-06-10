module Format.Builder.Code where

import Data.Monoid ((<>))
import Flow
import Format.Parser

import qualified Data.List as List
import qualified Data.Tuple as Tuple


-- 📮


buildList :: [Code] -> String
buildList pieces =
    pieces
        |> List.foldl reduce (Nothing, "")
        |> Tuple.snd



-- REDUCE
--
-- Reduce to a single entity


reduce :: (Maybe Code, String) -> Code -> (Maybe Code, String)
reduce (previousPiece, result) piece =
    [ --
      -- {bagage}
      --
      result

      --
      -- Prefix
      --
    , case previousPiece of
          Just prev     -> prefix prev piece
          Nothing       -> ""

      --
      -- Code
      --
    , case piece of
        Note (Comment _ 0 []) ->
            "--"

        Note (Comment _ 0 comment) ->
            "-- " <> comment

        Note (Comment newlines indentation []) ->
            ""
            <> List.replicate newlines '\n'
            <> List.replicate indentation ' '
            <> "--"

        Note (Comment newlines indentation comment) ->
            ""
            <> List.replicate newlines '\n'
            <> List.replicate indentation ' '
            <> "-- " <> comment

        Note (CommentBlock newlines indentation comment) ->
            ""
            <> List.replicate newlines '\n'
            <> List.replicate indentation ' '
            <> "{-" <> comment <> "-}"

        UnchartedLine line ->
            line

      --
      -- Suffix
      --
    , "\n"
    ]
        |> concat
        |> (,) (Just piece)



-- PREFIX
--
-- Generate a prefix based on two pieces of code


prefix :: Code -> Code -> String

{- Insert whitespace before a top-level `--` comment.
   If it is the last one that is.
-}
prefix (UnchartedLine _) (Note (Comment _ 0 _)) = "\n\n\n"

{- Insert whitespace after a top-level `--` comment -}
prefix (Note (Comment _ 0 _)) (Note Comment{}) = ""
prefix (Note (Comment _ 0 _)) _ = "\n\n"

{- No prefix otherwise -}
prefix _ _ = ""
