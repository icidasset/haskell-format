{-

THE PROCESSOR
=============

Processing the parsed Haskell files.

Wait what?
----------

Shift things around in a `Document`.
Stuff like reordering the imports, etc.

-}

module Format.Processor where

import Format.Parser (Document(..))


run :: Document -> Document
run document =
    document
