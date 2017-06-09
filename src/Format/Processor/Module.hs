module Format.Processor.Module where

import Format.Parser
import qualified Format.Processor.Portable


-- 📮


{-| Process the module part of a `Document`.

## Flow

Process the exports (aka. portables).

-}
process :: Module -> Module
process (Module name doc exports) =
    Module name doc (Format.Processor.Portable.processList exports)
