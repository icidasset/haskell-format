module Format.Parser.Module where

import Format.Parser.Portable
import Format.Parser.Utilities
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Maybe as Maybe


-- 🌳


{-| `Module` type.

1st argument = The name of the module
2nd argument = The stuff exported by the module

-}
data Module = Module String [Portable] deriving (Show)



-- 📮


{-| Parse the `module` bit of a document.

>>> parseTest docModule "module Example where"
Module "Example" []

>>> parseTest docModule "module Example (Type(..)) where"
Module "Example" [Portable "Type" [".."]]

>>> parseTest docModule "module Example\n    (a\n    , b\n    ) where"
Module "Example" [Portable "a" [],Portable "b" []]

-}
docModule :: Parser Module
docModule = do
    _               <- string "module"
    _               <- space
    moduleName      <- some letterChar
    _               <- space
    exports         <- optional portables
    _               <- space
    _               <- string "where"
    _               <- maybeSome spaceChar

    return $
        Module
            moduleName
            (Maybe.fromMaybe [] exports)
