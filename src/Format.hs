module Format
    ( Result(..)
    , format
    ) where

import Flow
import Format.Parser (Document(..))
import Text.Megaparsec (ParseError, ShowErrorComponent, ShowToken, parse)

import qualified Format.Builder as Builder
import qualified Format.Parser as Parser
import qualified Format.Processor as Processor
import qualified Text.Megaparsec as Mega (parseErrorPretty)


-- 🌳


data Result
    = Ok String
    | Err String



-- 📮


format :: String -> String -> Result
format contents filePath =
    contents
        |> parse Parser.document filePath
        |> either failure success



-- ⚗️


failure :: (Ord t, ShowToken t, ShowErrorComponent e) => ParseError t e -> Result
failure =
    Mega.parseErrorPretty .> Err


success :: Document -> Result
success =
    Processor.run .> Builder.run .> Ok
