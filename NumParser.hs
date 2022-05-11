{- Rewrite of ParseNum using AbLib.Control.Parser -}

{-
   This module provides functions for decoding Strings describing numbers into number types.
   Uses the Conway-Wechsler naming scheme.
-}

module NumberName.ParseNum where

import AbLib.Control.Parser
import AbLib.Control.ParserUtils (ws, digit)
import NumberName.NameNum
import Data.List ((\\), isSuffixOf)
import Data.Char (toLower)

{- Create Integral from a full name. -}
parseInt :: Integral a => Parser a
parseInt = do
   inputMap $ map toLower
   -- replacements [(",",""), ("illions","illion"), ("thousands","thousand")]
   intParser
   -- where

{- Parser for all integer values. -}
intParser :: Integral a => Parser a
intParser = mconcat
   [ match "negative" >> ws >> fmap negate intParser   -- negatives
   , fmap (const 0) $ match "zero"                             -- zero
   , Parser $ \s -> apply (posParser $ getOrder s) s ]         -- positives

{- Parser for integers strictly between 0 and 1000^(n+2). -}
posParser :: Integral a => a -> Parser a
posParser 0 = let
   name = matchOne ["thousand","nillion"]         -- generosity
   this = fmap (*1000) (digit3 << ws << name)
   next = mconcat [return 0, term, ws >> digit3]
   in digit3 <|> liftA2 (+) this next
posParser n = let
   factor = 1000 ^ (n + 1)
   this = fmap (factor*) (digit3 << ws << match (nameLarge n))
   next = mconcat [return 0, term, ws >> posParser (n-1)]
   in posParser (n-1) <|> liftA2 (+) this next

{- Parser for symbolic representation of numbers. -}
sym :: Integral a => Int -> Parser a
sym n = fmap (fromIntegral . read . reverse)
   $ exactly n digit

{- Parser for names of numbers. Practical only for small-scale. -}
parseByName :: Integral a => [a] -> Parser a
parseByName ns = mconcat [matchAs (nameInt n) n | n <- ns]

{- Parser for all forms of a 3-digit number. -}
digit3 :: Integral a => Parser a
digit3 = mempty
   <> digit2                     -- simpler case
   <> sym 3                      -- numeric case
   <> do                         -- general case
      hun <- fmap (*100) digit1     -- first digit
          << ws
          << match "hundred"
      ten <- return 0 <> term       -- second, third digit
      return (hun + ten)

{- Parser for the last two digits of any number. -}
term :: Integral a => Parser a
term = ws >> match "and" >> ws >> digit2

{- Parser for all forms of a 2-digit number. -}
digit2 :: Integral a => Parser a
digit2 = mempty
   <> digit1                              -- simpler case
   <> sym 2                               -- numeric case
   <> parseByName [10..19]                -- matches teens
   <> do                                  -- general case
      x1 <- parseByName [20,30..90]          -- first digit
      ws <> match "-"                        -- digit joiner
      x0 <- parseByName [1..9] <> return 0   -- last digit
      return (x1 + x0)

{- Parser for all forms of a 1-digit number. -}
digit1 :: Integral a => Parser a
digit1 = sym 1 <|> parseByName [1..9]

{- Computes the order of a named number by finding it's first "zillion". -}
getOrder :: Integral a => String -> a
getOrder s = case filter (isSuffixOf "illion") $ words s of
   [ ] -> 0
   [x] -> fullParse parseLarge x
   
-- Parser for large number orders (illion).
-- Inverse of nameLarge.
parseLarge :: Integral a => Parser a
parseLarge = do
   inputMap (map toLower)
   matchAs "thousand" 0 <|> largeParser
   where

   {- Parser for large numbers. -}
   largeParser :: Integral a => Parser a
   largeParser = liftA2 (flip ($)) initial largeParser'
      where
      initial :: Integral a => Parser a
      initial = fmap (*1000) prefix << match "lli"
      largeParser' :: Integral a => Parser (a -> a)
      largeParser' = matchAs "on" (`div` 1000) <|> do
         x <- initial
         f <- largeParser'
         return $ \z -> f (1000*z+x)

   {- Prefixes for large 0-999. -}
   prefix :: Integral a => Parser a
   prefix = let
      prefix0 = mconcat $ map (uncurry matchAs) $ zip (table !! 0) [0..9]
      prefix2 = mconcat $ map (uncurry matchAs) $ zip (table !! 2) [10,20..90]
      prefix3 = mconcat $ map (uncurry matchAs) $ zip (table !! 3) [100,200..900]
      combine = liftA2 (+) prefix2 prefix3
      in prefix0 <|> liftA2 (+) prefix1 (prefix2 <|> prefix3 <|> combine)
   
   {- Unit prefixes for large 10-999. The units part is more involved, hence this function. -}
   prefix1 :: Integral a => Parser a
   prefix1 = mconcat
      [ return             0
      , matchAs "un"       1
      , matchAs "duo"      2
      , matchAs "tres"     3 << peek (matchOne "coqtv")
      , matchAs "tre"      3 << peek (matchOne "abdefghijklmnprsuwxyz")
      , matchAs "quattuor" 4
      , matchAs "quin"     5
      , matchAs "sex"      6 << peek (matchOne "co")
      , matchAs "ses"      6 << peek (matchOne "qtv")
      , matchAs "se"       6 << peek (matchOne "abdefghijklmnprsuwxyz")
      , matchAs "septen"   7 << peek (matchOne "cdqst")
      , matchAs "septem"   7 << peek (matchOne "ov")
      , matchAs "septe"    7 << peek (matchOne "abefghijklmnpruwxyz")
      , matchAs "octo"     8
      , matchAs "noven"    9 << peek (matchOne "cdqst")
      , matchAs "novem"    9 << peek (matchOne "ov")
      , matchAs "nove"     9 << peek (matchOne "abefghijklmnpruwxyz")    ]
   
   {- Simple table of prefixes. -}
   table :: [[String]]
   table =
      [ [ "ni", "mi",     "bi",           "tri"
        , "quadri",       "quinti",       "sexti"
        , "septi",        "octi",         "noni"      ]
      , [ {- This row is intentionally left empty. -} ]
      , [ "deci",         "viginti",      "triginta"
        , "quadraginta",  "quinquaginta", "sexaginta"
        , "septuaginta",  "octoginta",    "nonaginta" ]
      , [ "centi",        "ducenti",      "trecenti"
        , "quadringenti", "quingenti",    "sescenti"
        , "septingenti",  "octingenti",   "nongenti"  ] ]
