module Format
    ( format
    ) where

import qualified Format.Parser
import qualified Text.Megaparsec (parseTest)


format :: IO ()
format = do
    contents <- readFile "./tests/fixtures/Example.hs"
    Text.Megaparsec.parseTest Format.Parser.document contents
