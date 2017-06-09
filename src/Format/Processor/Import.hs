module Format.Processor.Import where

import Flow
import Format.Parser

import qualified Data.List as List (map, sortBy)
import qualified Format.Processor.Portable


-- 📮


{-| Process the imports of a `Document`.

## Flow

Sort the imports by:
1. Qualified or not qualified
2. The name of the import

&

Process the portables

-}
process :: [Import] -> [Import]
process imports =
    imports
        |> List.sortBy compareImports
        |> List.map processPortables


compareImports :: Import -> Import -> Ordering
compareImports (Import name_of_a opts_of_a _) (Import name_of_b opts_of_b _) =
    case compare (qualified opts_of_a) (qualified opts_of_b) of
        EQ -> compare name_of_a name_of_b
        LT -> LT
        GT -> GT


processPortables :: Import -> Import
processPortables (Import name options portables) =
    Import name options (Format.Processor.Portable.processList portables)
