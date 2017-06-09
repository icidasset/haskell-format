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

import Flow
import Format.Parser

import qualified Format.Processor.Import
import qualified Format.Processor.Module


-- 📮


run :: Document -> Document
run doc =
    doc
        |> mapModule Format.Processor.Module.process
        |> mapImports Format.Processor.Import.process



-- ⚗️


mapModule :: (Module -> Module) -> Document -> Document
mapModule mapFn (Document m i c) = Document (mapFn m) i c


mapImports :: ([Import] -> [Import]) -> Document -> Document
mapImports mapFn (Document m i c) = Document m (mapFn i) c


mapCode :: ([Code] -> [Code]) -> Document -> Document
mapCode mapFn (Document m i c) = Document m i (mapFn c)
