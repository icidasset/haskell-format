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
Module "Example" Nothing [Portable "Type" [".."]]

>>> parseTest docModule "module Example\n    (a\n    , b\n    ) where"
Module "Example" Nothing [Portable "a" [],Portable "b" []]

>>> parseTest docModule (['{', '-'] ++ "| Hi! " ++ ['-', '}'] ++ "\n module Example where")
Module "Example" (Just (CommentBlock "| Hi! ")) []

-}
docModule :: Parser Module
docModule = do
    moduleDocs      <- optional multiLineComment
    _               <- maybeSome spaceChar
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
            moduleDocs
            (Maybe.fromMaybe [] exports)