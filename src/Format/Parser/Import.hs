module Format.Parser.Import where

import Format.Parser.Portable
import Format.Parser.Utilities
import Prelude hiding (or)
import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Maybe as Maybe


-- 🌳


{-| `Import` type.

1st argument = The module to import
2nd argument = Import options (see type below)
3rd argument = The stuff we chose to import

-}
data Import = Import String ImportOptions [Portable] deriving (Show)


{-| `ImportOptions` type.
-}
data ImportOptions =
    ImportOptions
        { alias :: Maybe String
        , hiding :: Bool
        , qualified :: Bool
        }
        deriving (Show)


defaultOptions :: ImportOptions
defaultOptions =
    ImportOptions
        { alias = Nothing
        , hiding = False
        , qualified = False
        }



-- 📮


{-| Parse the `imports` of a document.

>>> parseTest docImport "import A"
Import "A" (ImportOptions {alias = Nothing, hiding = False, qualified = False}) []

>>> parseTest docImport "import A (c, d)"
Import "A" ... [Portable "c" [],Portable "d" []]

>>> parseTest docImport "import qualified B"
Import "B" (ImportOptions {alias = Nothing, hiding = False, qualified = True}) []

>>> parseTest docImport "import qualified B (C(D))"
Import "B" ... [Portable "C" ["D"]]

>>> parseTest docImport "import A.B.C hiding ()"
Import "A.B.C" (ImportOptions {alias = Nothing, hiding = True, qualified = False}) []

>>> parseTest docImport "import A as B"
Import "A" (ImportOptions {alias = Just "B", hiding = False, qualified = False}) []

-}
docImport :: Parser Import
docImport = do
    _               <- one (string "import")
    _               <- some whitespace
    isQualified     <- optional (string "qualified")
    _               <- maybeSome whitespace
    importName      <- one moduleName
    _               <- maybeSome whitespace
    theOptions      <- options
    _               <- maybeSome whitespace
    dependencies    <- optional portables
    _               <- maybeSome whitespace

    return $
        Import
            importName
            (theOptions { qualified = Maybe.isJust isQualified })
            (Maybe.fromMaybe [] dependencies)



-- Options


{-| Build the `ImportOptions`.

    Checks for the keywords `as` and `hiding`.

-}
options :: Parser ImportOptions
options =
    choice [ keyAlias, keyHiding, keyDefault ]


keyAlias :: Parser ImportOptions
keyAlias = do
    _               <- one (string "as ")
    theAlias        <- one moduleName

    return defaultOptions { alias = Just theAlias }


keyHiding :: Parser ImportOptions
keyHiding = do
    _               <- one (string "hiding")

    return defaultOptions { hiding = True }


keyDefault :: Parser ImportOptions
keyDefault =
    return defaultOptions
