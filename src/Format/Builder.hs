{-

THE BUILDER
===========

Rebuilding the parsed & processed document into a text/string format.

Wait what?
----------

`Document` -> `String`.

The idea
--------

Rebuild the whole document from scratch.

-}

module Format.Builder where

import Flow
import Format.Parser

import qualified Data.List as List (intercalate, map)
import qualified Format.Builder.Code
import qualified Format.Builder.Import
import qualified Format.Builder.Module


-- 📮


run :: Document -> String
run (Document theModule theImports piecesOfCode) = concat
    [ -- Module
      Format.Builder.Module.build theModule

      -- {whitespace}
    , "\n\n"

      -- Imports
    , theImports
        |> List.map Format.Builder.Import.build
        |> List.intercalate "\n"

      -- {whitespace}
    , "\n\n\n"

      -- Code
    , Format.Builder.Code.buildList piecesOfCode
    ]
