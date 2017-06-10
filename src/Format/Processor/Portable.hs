module Format.Processor.Portable where

import Flow
import Format.Parser

import qualified Data.Char as Char (isUpper)
import qualified Data.List as List (head, map, sort, sortBy)


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


{-| Compare portables.

>>> List.sortBy comparePortables [Portable "(~>)" [], Portable "A" []]
[Portable "A" [],Portable "(~>)" []]

>>> List.sortBy comparePortables [Portable "a" [], Portable "A" []]
[Portable "A" [],Portable "a" []]

>>> List.sortBy comparePortables [Portable "Z" [], Portable "A" []]
[Portable "A" [],Portable "Z" []]

>>> List.sortBy comparePortables [Portable "z" [], Portable "a" []]
[Portable "a" [],Portable "z" []]

-}
comparePortables :: Portable -> Portable -> Ordering
comparePortables (Portable name_of_a _) (Portable name_of_b _) =
    case (isUpper name_of_a, isUpper name_of_b) of
        (True, True) -> compare name_of_a name_of_b
        (False, False) -> compare name_of_a name_of_b
        (True, False) -> LT
        (False, True) -> GT


processDataConstructors :: Portable -> Portable
processDataConstructors (Portable name constructors) =
    Portable name (List.sort constructors)



-- Utilities


isUpper :: String -> Bool
isUpper =
    List.head .> Char.isUpper
