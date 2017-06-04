module Format.Parser.Comment where

import Format.Parser.Utilities
import Prelude hiding (or)
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


{-| A comment, block or non-block, ignores surround whitespace.

>>> parseTest comment "\n\n-- Hi!\n\n\n"
Comment "Hi!"

-}
comment :: Parser Comment
comment = do
    _           <- maybeSome spaceChar
    theComment  <- singleLineComment `or` multiLineComment
    _           <- maybeSome spaceChar

    return theComment


{-| A single-line comment, starts `with --`.

>>> parseTest singleLineComment "    -- Girl I didn't know you could go down like that"
Comment "Girl I didn't know you could go down like that"

>>> parseTest singleLineComment "--\n"
Comment ""

-}
singleLineComment :: Parser Comment
singleLineComment = do
    _           <- maybeSome separatorChar
    _           <- string "--"
    _           <- optional (char ' ')
    theComment  <- try (manyTill anyChar eol) `or` maybeSome anyChar

    return $ Comment theComment


{-| A multi-line comment, like this one.

>>> parseTest multiLineComment ['{', '-', '-', '}']
CommentBlock ""

>>> parseTest multiLineComment ['{', '-', 'x', 'y', 'z', '-', '}']
CommentBlock "xyz"

>>> parseTest multiLineComment ['{', '-', 'x', '\n', 'z', '-', '}']
CommentBlock "x\nz"

-}
multiLineComment :: Parser Comment
multiLineComment = do
    _           <- maybeSome separatorChar
    _           <- string "{-"
    theComment  <- manyTill anyChar (string "-}")

    return $ CommentBlock theComment
