module Format.Parser.Comment where

import Flow
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
    = Comment Int String
    | CommentBlock Int String
    deriving (Show)



-- 📮


{-| A comment, one of the options below this function.

>>> parseTest comment "\n\n-- Hi!\n\n\n"
Comment 0 "Hi!"

-}
comment :: Parser Comment
comment =
    singleLineComment `or` multiLineComment


{-| A single-line comment, starts `with --`.

⚠️ Ignores leading new-lines and trailing in some cases.

>>> parseTest singleLineComment "    -- Girl I didn't know you could go down like that"
Comment 4 "Girl I didn't know you could go down like that"

>>> parseTest singleLineComment "-- \n"
Comment 0 ""

-}
singleLineComment :: Parser Comment
singleLineComment = do
    spaceBefore     <- whitespace
    _               <- string "-- "                          -- <start>
    theComment      <- manyTill anyChar singleLineEnding    -- ... </end>

    -- Result
    theComment
        |> Comment (leadingSpace spaceBefore)
        |> makeParser


singleLineEnding :: Parser String
singleLineEnding =
    choice
        [ eol
        , fmap (const "") eof
        ]



{-| A multi-line comment, like this one.

⚠️ Ignores leading new-lines and trailing in some cases.

>>> parseTest multiLineComment ['{', '-', '-', '}']
CommentBlock 0 ""

>>> parseTest multiLineComment ['{', '-', 'x', 'y', 'z', '-', '}']
CommentBlock 0 "xyz"

>>> parseTest multiLineComment ['{', '-', 'x', '\n', 'z', '-', '}']
CommentBlock 0 "x\nz"

-}
multiLineComment :: Parser Comment
multiLineComment = do
    spaceBefore     <- whitespace
    _               <- string "{-"                          -- <start>
    theComment      <- manyTill anyChar multiLineEnding     -- ... </end>

    -- Result
    theComment
        |> CommentBlock (leadingSpace spaceBefore)
        |> makeParser


multiLineEnding :: Parser String
multiLineEnding =
    string "-}"



-- ⚗️


makeParser :: Comment -> Parser Comment
makeParser c =
    let
        indentation =
            case c of
                Comment ind _ -> ind
                CommentBlock ind _ -> ind
    in
        -- Remove trailing whitespace
        -- {!} if the leading-space count is zero
        -- >>> and then return
        if indentation == 0 then whitespace >> return c
        else return c
