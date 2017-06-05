module Format.Builder.Import where

import Flow
import Format.Parser

import qualified Data.List as List (intercalate, map)
import qualified Format.Builder.Portable as Portable


-- 📮


{-| Build an `Import`.

```
import qualified Name (Portable)
```

-}
build :: Import -> String
build (Import name isQualified portables) = concat
    [ -- Declaration
      "import "

      -- Qualified?
    , if isQualified then "qualified " else ""

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
