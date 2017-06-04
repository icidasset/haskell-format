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

import Format.Parser (Document(..))


run :: Document -> String
run document =
    show document
