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
    , render piece

      --
      -- Suffix
      --
    , suffix piece
    ]
        |> concat
        |> (,) (Just piece)



-- PREFIX
--
-- Generate a prefix based on two pieces of code


prefix :: Code -> Code -> String

{- Insert whitespace before a top-level `--` comment.
   If it is the first one that is.
-}
prefix (UnchartedLine _) (Note (Comment _ 0 _)) = "\n\n\n"
prefix (Specification _ _) (Note (Comment _ 0 _)) = "\n\n\n"

{- Insert whitespace after a top-level `--` comment -}
prefix (Note (Comment _ 0 _)) (Note Comment{}) = ""
prefix (Note (Comment _ 0 _)) _ = "\n\n"

{- Insert two empty lines before certain code -}
prefix (Note (CommentBlock _ _ _)) (Specification _ _) = ""
prefix _ (Specification _ _) = "\n\n"

{- No prefix otherwise -}
prefix _ _ = ""



-- CODE
--
-- Render a single piece of code


render :: Code -> String



-- CODE : Notes


render (Note (Comment _ 0 [])) =
    "--"

render (Note (Comment _ 0 comment)) =
    "-- " <> comment

render (Note (Comment newlines indentation [])) =
    ""
    <> List.replicate newlines '\n'
    <> List.replicate indentation ' '
    <> "--"

render (Note (Comment newlines indentation comment)) =
    ""
    <> List.replicate newlines '\n'
    <> List.replicate indentation ' '
    <> "-- " <> comment

render (Note (CommentBlock newlines indentation comment)) =
    ""
    <> List.replicate newlines '\n'
    <> List.replicate indentation ' '
    <> "{-" <> comment <> "-}"



-- CODE : Level 1


render (Specification name typ) =
    name <> " :: " <> typ


render (Definition name) =
    name <> " = "



-- CODE : End of the line


render (UnchartedLine line) =
    line



-- SUFFIX
--
-- Generate a suffix based the code


suffix :: Code -> String

{- No suffix for Definitions -}
suffix (Definition _) = ""

{- Default suffix otherwise -}
suffix _ = "\n"
