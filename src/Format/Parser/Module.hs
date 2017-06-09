module Format.Parser.Module where

import Format.Parser.Comment
import Format.Parser.Portable
import Format.Parser.Utilities
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Maybe as Maybe


-- 🌳


{-| `Module` type.

1st argument = The name of the module
2nd argument = The documentation of the module
3rd argument = The stuff exported by the module

-}
data Module = Module String (Maybe Comment) [Portable] deriving (Show)



-- 📮


{-| Parse the `module` bit of a document.

>>> parseTest docModule "module Example where"
Module "Example" Nothing []

>>> parseTest docModule "module Example (Type(..)) where"
Module "Example" Nothing [...]

>>> parseTest docModule "module Example\n    (a\n    , b\n    ) where"
Module "Example" Nothing [...]

>>> parseTest docModule (['{', '-', '-', '}'] ++ "\n module Example where")
Module "Example" (Just ...) []

-}
docModule :: Parser Module
docModule = do
    docs            <- optional multiLineComment
    _               <- one (string "module")
    _               <- some whitespace
    name            <- one moduleName
    _               <- some whitespace
    exports         <- optional portables
    _               <- maybeSome whitespace
    _               <- one (string "where")
    _               <- maybeSome whitespace

    return $
        Module
            name
            docs
            (Maybe.fromMaybe [] exports)
