module Format.Builder.Import where

import Data.Monoid ((<>))
import Flow
import Format.Parser

import qualified Data.List as List (filter, intercalate, map)
import qualified Format.Builder.Portable as Portable


-- 📮


{-| Build an `Import` list
-}
buildList :: [Import] -> String
buildList list =
    let
        listNonQualified =
            List.filter (isQualified .> (==) False) list

        listQualified =
            List.filter (isQualified .> (==) True) list
    in
        concat
            [ -- Non-qualified
              --
              listNonQualified
                |> List.map build
                |> List.intercalate "\n"

              --
              --
            , if length list > 2 && not (null listQualified) then
                "\n\n"
              else if not (null listQualified) then
                "\n"
              else
                ""

              -- Qualified
              --
            , listQualified
                |> List.map build
                |> List.intercalate "\n"
            ]


{-| Build an `Import`.

```
import qualified Name (Portable)
```

-}
build :: Import -> String
build (Import name options portables) = concat
    [ -- Declaration
      "import "

      -- Qualified?
    , if qualified options then "qualified " else ""

      -- Name & Options
    , name
    , keywords options

      -- Portables
    , case portables of
        [] ->
            case options of
                ImportOptions _ True _ _ ->
                    " ()"

                _ ->
                    ""

        list ->
            concat
                [ " "
                , "("
                , list
                    |> List.map Portable.build
                    |> List.intercalate ", "
                , ")"
                ]
    ]


{-| Is-qualified helper.
-}
isQualified :: Import -> Bool
isQualified (Import _ options _) = qualified options



-- Keywords


keywords :: ImportOptions -> String
keywords (ImportOptions (Just anAlias) _ _ _) = " as " <> anAlias
keywords (ImportOptions _ _ True _) = " hiding"
keywords _ = ""
