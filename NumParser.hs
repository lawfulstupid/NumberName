
-- Rewrite of NumName using AbLib.Control.Parser
module NumberName.NumParser where

import AbLib.Control.Parser
import qualified AbLib.Control.ParserUtils as P

nameInt :: (Show a, Integral a) => a -> Maybe String
nameInt = maybeParse intParser . show

intParser :: Parser String
intParser = match "we"