module Format.Builder.Import where

import Flow
import Format.Parser.Import (Import(..))

import qualified Data.List as List (intercalate, map)
import qualified Format.Builder.Portable as Portable


-- 📮


build :: Import -> String
build (Import name isQualified portables) = concat
    [ -- Declaration
      "import "

      -- Qualified?
    , case isQualified of
        True    -> "qualified "
        False   -> ""

      -- Name
    , name

      -- Portables
    , case portables of
        [] ->
            ""

        list ->
            concat
                [ " ("
                , list
                    |> List.map Portable.build
                    |> List.intercalate ", "
                , ")"
                ]
    ]
