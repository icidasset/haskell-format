module Format.Builder.Portable where

import Data.Monoid ((<>))
import Format.Parser

import qualified Data.List as List (intercalate)


-- 📮


{-| Build a `Portable`.

```
PortableWithDataConstructors(A, B, C)
```

-}
build :: Portable -> String
build (Portable name dataConstructors) = concat
    [ -- Name
      name

      -- Data constructors
    , case dataConstructors of
        [] ->
            ""

        list ->
            "(" <> List.intercalate ", " list <> ")"
    ]
