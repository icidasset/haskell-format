module Format.Parser.Code where

import Format.Parser.Comment
import Text.Megaparsec
import Text.Megaparsec.String


-- 🌳


data Code
    = Note Comment
    --
    -- Uncharted territory
    -- > ie. an unparsed line of code
    | UnchartedLine String
    deriving (Show)



-- 📮


code :: Parser Code
code =
    choice
        [ try (fmap Note comment)
        , unchartedLine
        ]


unchartedLine :: Parser Code
unchartedLine = do
    line <- manyTill anyChar eol

    return $ UnchartedLine line
