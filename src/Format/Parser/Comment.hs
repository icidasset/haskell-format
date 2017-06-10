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

1st argument = The leading new-lines
2nd argument = The leading spaces on the line where the comment starts
3rd argument = The comment itself

-}
data Comment
    = Comment Int Int String
    | CommentBlock Int Int String
    deriving (Show)



-- 📮


{-| A comment, one of the options below this function.

>>> parseTest comment "\n\n-- Hi!\n\n\n"
Comment 2 0 "Hi!"

-}
comment :: Parser Comment
comment =
    singleLineComment `or` multiLineComment


{-| A single-line comment, starts `with --`.

>>> parseTest singleLineComment "    -- Girl I didn't know you could go down like that"
Comment 0 4 "Girl I didn't know you could go down like that"

>>> parseTest singleLineComment "-- \n"
Comment 0 0 ""

-}
singleLineComment :: Parser Comment
singleLineComment = do
    spaceBefore     <- maybeSome whitespace
    _               <- one (string "--")                    -- <start>
    _               <- maybeSome spaceCharacter
    theComment      <- manyTill anyChar singleLineEnding    -- ... </end>

    -- Result
    theComment
        |> Comment (leadingNewlines spaceBefore) (leadingSpace spaceBefore)
        |> makeParser


singleLineEnding :: Parser String
singleLineEnding =
    choice
        [ eol
        , fmap (const "") eof
        ]



{-| A multi-line comment, like this one.

>>> parseTest multiLineComment ['{', '-', '-', '}']
CommentBlock 0 0 ""

>>> parseTest multiLineComment ['{', '-', 'x', 'y', 'z', '-', '}']
CommentBlock 0 0 "xyz"

>>> parseTest multiLineComment ['{', '-', 'x', '\n', 'z', '-', '}']
CommentBlock 0 0 "x\nz"

-}
multiLineComment :: Parser Comment
multiLineComment = do
    spaceBefore     <- maybeSome whitespace
    _               <- one (string "{-")                    -- <start>
    theComment      <- manyTill anyChar multiLineEnding     -- ... </end>

    -- Result
    theComment
        |> CommentBlock (leadingNewlines spaceBefore) (leadingSpace spaceBefore)
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
                Comment _ ind _ -> ind
                CommentBlock _ ind _ -> ind
    in
        -- Remove trailing whitespace
        -- {!} if the leading-space count is zero
        -- >>> and then return
        if indentation == 0 then maybeSome whitespace >> return c
        else return c
