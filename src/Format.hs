module Format
    ( format
    ) where

import Flow
import Format.Parser (Document(..))
import Text.Megaparsec (parse, parseErrorPretty)

import qualified Format.Builder as Builder
import qualified Format.Parser as Parser
import qualified Format.Processor as Processor


-- 🍯


format :: String -> IO ()
format relativePath =
    relativePath
        |> readFile
        |> fmap (runParser relativePath)
        |> anew (putStr)



-- ⚗️


anew :: Monad m => (a -> m b) -> m a -> m b
anew =
    flip (>>=)


runParser :: String -> String -> String
runParser relativePath content =
    content
        |> parse Parser.document relativePath
        |> either parseErrorPretty handleDocument


handleDocument :: Document -> String
handleDocument =
    Processor.run >> Builder.run
