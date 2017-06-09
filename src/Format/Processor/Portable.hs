module Format.Processor.Portable where

import Flow
import Format.Parser
import qualified Data.List as List (map, sort, sortBy)


-- 📮


{-| Process a list of `Portable`s.

## Flow

Sort the list by A-Z first and then by a-z.

-}
processList :: [Portable] -> [Portable]
processList list =
    list
        |> List.sortBy comparePortables
        |> List.map processDataConstructors


comparePortables :: Portable -> Portable -> Ordering
comparePortables (Portable name_of_a _) (Portable name_of_b _) =
    compare name_of_a name_of_b


processDataConstructors :: Portable -> Portable
processDataConstructors (Portable name constructors) =
    Portable name (List.sort constructors)
