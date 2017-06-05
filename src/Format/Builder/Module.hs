module Format.Builder.Module where

import Data.Monoid ((<>))
import Flow
import Format.Parser

import qualified Data.List as List (intercalate, map)
import qualified Format.Builder.Portable as Portable


-- 📮


{-| Build a `Module`.

```
module Example
    ( exportA
    , exportB
    ) where
```

-}
build :: Module -> String
build (Module name documentation portables) = concat
    [ -- Documentation
      case documentation of
        Just (CommentBlock comment) ->
            "{-" <> comment <> "-}\n"
        _ ->
            ""

      -- Declaration
    , "module " <> name <> " "

      -- Portables
    , case portables of
        [] ->
            ""

        list ->
            concat
                [ "\n    ( "
                , list
                    |> List.map Portable.build
                    |> List.intercalate "\n    , "
                , "\n    )"
                ]

      -- The end
    , " where"
    ]
