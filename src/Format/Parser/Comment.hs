module Format.Parser.Comment where

import Format.Parser.Utilities
import Prelude hiding (and, or)
import Text.Megaparsec
import Text.Megaparsec.String


-- 🌳


{-| `Comment` type.

Which is either an single-line comment
or a multi-line (block) comment.

-}
data Comment
    = Comment String
    | CommentBlock String
    deriving (Show)



-- 📮


{-| A comment, one of the options below this function.

>>> parseTest comment "\n\n-- Hi!\n\n\n"
Comment "Hi!"

-}
comment :: Parser Comment
comment =
    singleLineComment `or` multiLineComment


{-| A single-line comment, starts `with --`.

⚠️ Ignores surrounding whitespace

>>> parseTest singleLineComment "    -- Girl I didn't know you could go down like that"
Comment "Girl I didn't know you could go down like that"

>>> parseTest singleLineComment "--\n"
Comment ""

-}
singleLineComment :: Parser Comment
singleLineComment = do
    _           <- whitespace
    _           <- string "--"                          -- <start>
    _           <- spaceCharacters                      -- Remove leading spaces
    theComment  <- manyTill anyChar singleLineEnding    -- ... </end>
    _           <- whitespace

    return $ Comment theComment


singleLineEnding :: Parser String
singleLineEnding =
    choice
        [ eol
        , fmap (const "") eof
        ]



{-| A multi-line comment, like this one.

⚠️ Ignores surrounding whitespace

>>> parseTest multiLineComment ['{', '-', '-', '}']
CommentBlock ""

>>> parseTest multiLineComment ['{', '-', 'x', 'y', 'z', '-', '}']
CommentBlock "xyz"

>>> parseTest multiLineComment ['{', '-', 'x', '\n', 'z', '-', '}']
CommentBlock "x\nz"

>>> parseTest multiLineComment ['{', '-', '|', ' ', ' ', '-', '}']
CommentBlock ""

-}
multiLineComment :: Parser Comment
multiLineComment = do
    _           <- whitespace
    _           <- string "{-"                          -- `{-`
    _           <- optional (char '|')                  -- Remove `|`
    _           <- whitespace                           -- Remove leading spaces
    theComment  <- manyTill anyChar multiLineEnding     -- ... `-}`
    _           <- whitespace

    return $ CommentBlock theComment


multiLineEnding :: Parser String
multiLineEnding =
    spaceCharacters `andThen` string "-}"               -- Remove trailing spaces
