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

import Format.Parser

import qualified Data.List as List (null)
import qualified Format.Builder.Code
import qualified Format.Builder.Import
import qualified Format.Builder.Language
import qualified Format.Builder.Module


-- 📮


run :: Document -> String
run (Document languagePragmas theModule theImports piecesOfCode) = concat
    [ -- Language Pragmas
      Format.Builder.Language.buildList languagePragmas

      --
     , if List.null languagePragmas then
         ""
       else
         "\n"

      -- Module
    , Format.Builder.Module.build theModule

      --
    , "\n\n"

      -- Imports
    , Format.Builder.Import.buildList theImports

      --
    , "\n\n\n"

      -- Code
    , Format.Builder.Code.buildList piecesOfCode
    ]
