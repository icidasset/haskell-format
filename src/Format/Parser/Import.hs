module Format.Parser.Import where

import Format.Parser.Portable
import Format.Parser.Utilities
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Maybe as Maybe


-- 🌳


{-| `Import` type.

1st argument = The module to import
2nd argument = Is this a qualified import?
3rd argument = The stuff we chose to import

-}
data Import = Import String Bool [Portable] deriving (Show)



-- 📮


{-| Parse the `imports` of a document.

>>> parseTest docImport "import A"
Import "A" False []

>>> parseTest docImport "import A (c, d)"
Import "A" False [Portable "c" [],Portable "d" []]

>>> parseTest docImport "import qualified B"
Import "B" True []

>>> parseTest docImport "import qualified B (C(D))"
Import "B" True [Portable "C" ["D"]]

-}
docImport :: Parser Import
docImport = do
    _               <- string "import"
    _               <- space
    isQualified     <- optional (string "qualified")
    _               <- space
    importName      <- some letterChar
    _               <- space
    dependencies    <- optional portables
    _               <- maybeSome spaceChar

    return $
        Import
            importName
            (Maybe.isJust isQualified)
            (Maybe.fromMaybe [] dependencies)
