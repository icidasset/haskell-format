module Format.Builder.Import where

import Data.Monoid ((<>))
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



-- Keywords


keywords :: ImportOptions -> String
keywords (ImportOptions (Just anAlias) _ _) = " as " <> anAlias
keywords (ImportOptions _ True _) = " hiding "
keywords _ = ""
