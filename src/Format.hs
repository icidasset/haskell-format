module Format
    ( format
    ) where

import Flow
import Format.Parser (Document(..))
import Text.Megaparsec as Mega (parse, parseErrorPretty)

import qualified Format.Builder as Builder
import qualified Format.Parser as Parser
import qualified Format.Processor as Processor


-- 🍯


format :: String -> IO ()
format relativePath =
    relativePath
        |> readFile
        |> fmap (parse Parser.document relativePath)
        |> fmap (either Mega.parseErrorPretty handleDocument)
        |> anew putStr



-- ⚗️


anew :: Monad m => (a -> m b) -> m a -> m b
anew =
    (=<<)


handleDocument :: Document -> String
handleDocument =
    Processor.run >> Builder.run
