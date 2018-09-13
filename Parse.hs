-- This module provides functions for decoding Strings describing numbers into number types.
-- Uses the Conway-Wechsler naming scheme.
module NumberName.Parse where

-- TODO: Learn Parsec and do this properly

import NumberName.Name
import NumberName.ParserUtils
import Data.List ((\\), isSuffixOf)
import Data.Char (toLower)

-- DIGIT := one | two | three | four | five | six | seven | eight | nine
-- TEENS := ten | eleven | twelve | thirteen | fourteen | fifteen | sixteen | seventeen | eighteen | nineteen
-- TENS  := twenty | thirty | forty | fifty | sixty | seventy | eighty | ninety

-- D2 := DIGIT | TEENS | TENS | TENS DIGIT
-- NT := and D2 | ""

-- D3 := D2 | DIGIT hundred NT

-- T0 := D3
-- T1 := T0 | D3 thousand T0 | D3 thousand NT
-- T2 := T1 | D3 million T1  | D3 million NT
-- T3 := T2 | D3 billion T2  | D3 billion NT
-- et cetera

{- Create Integral from a full name. -}
parseInt :: Integral a => String -> a
parseInt = parser intParser
   . preprocess [(",",""), ("illions","illion"), ("thousands","thousand")]
   . map toLower
   where

   {- Parser for all integer values. -}
   intParser :: Integral a => Parser a
   intParser = parallel
      [ match "negative" & spaces & intParser ~> negate  -- negatives
      , match "zero" ~> const 0                          -- zero
      , \s -> posParser (getOrder s) s                 ] -- positives
   
   {- Parser for integers strictly between 0 and 1000^(n+2). -}
   posParser :: Integral a => a -> Parser a
   posParser 0 = let
      name = match "thousand" <|> match "nillion"         -- generosity
      this = digit3 <& spaces <& name ~> (* 1000)
      next = accept 0 <|> term <|> (spaces & digit3)
      in (this <&> next ~> uncurry (+)) <|> digit3
   posParser n = let
      factor = 1000 ^ (n + 1)
      this = digit3 <& spaces <& match (nameLarge n) ~> (* factor)
      next = accept 0 <|> term <|> (spaces & posParser (n-1))
      in (this <&> next ~> uncurry (+)) <|> posParser (n-1)
   
   {- Parser for symbolic representation of numbers. -}
   sym :: (Integral a, Integral b, Show a) => [a] -> Parser b
   sym ns = options [(show n, fromInteger $ toInteger n) | n <- ns]
      -- need to convert between Integral types to avoid Show constraint.
   
   {- Parser for names of numbers. Practical only for small-scale. -}
   parseExact :: (Integral a) => [a] -> Parser a
   parseExact ns = options [(nameInt n, n) | n <- ns]
   
   {- Parser for all forms of a 3-digit number. -}
   digit3 :: Integral a => Parser a
   digit3 = let
      hun = digit1 <& spaces <& match "hundred" ~> (100*)   -- match "`x` hundred" as 100*x
      -- Parser enumerates possible forms of a 3-digit number:
      in parallel
         [ sym [100..999]                 -- symbolic representation
         , digit2                         -- 2-digit number
         , hun <&> term ~> uncurry (+) ]  -- full three-digit number
      
   {- Parser for final two digits non-inital digits. -}
   term :: Integral a => Parser a
   term = (optional (spaces & match "and") & spaces & digit2) <|> accept 0
   
   {- Parser for all forms of a 2-digit number. -}
   digit2 :: Integral a => Parser a
   digit2 = let
      tens = parseExact [20,30..90]       -- match strict multiples of ten
      join = space <|> match "-"
      unit = accept 0 <|> (join & digit1)
      -- Parser enumerates possible forms of a 2-digit number:
      in parallel
         [ sym [10..99]                   -- symbolic representation
         , digit1                         -- 1-digit number
         , parseExact [10..19]            -- teen number
         , tens <&> unit ~> uncurry (+) ] -- "`x`ty `y`"
   
   {- Parser for all forms of a 1-digit number. -}
   digit1 :: Integral a => Parser a
   digit1 = sym [1..9] <|> parseExact [1..9]
   
   {- Computes the order of a named number by finding it's first "zillion". -}
   getOrder :: Integral a => String -> a
   getOrder s = case filter (isSuffixOf "illion") (words s) of
      [ ] -> 0
      [x] -> parseLarge x
   
-- Parser for large number orders (illion).
-- Inverse of nameLarge.
parseLarge :: Integral a => String -> a
parseLarge = parser ((match "thousand" ~> const 0) <|> largeParser) . map toLower
   where

   {- Parser for large numbers. -}
   largeParser :: Integral a => Parser a
   largeParser = initial <&> largeParser' ~> \(x,f) -> f x
      where
      initial :: Integral a => Parser a
      initial = prefix <& match "lli" ~> (*1000)
      largeParser' :: Integral a => Parser (a -> a)
      largeParser' = parallel
         [ match "on"               ~> const (`div` 1000)
         , initial <&> largeParser' ~> \ (x,f) z -> f (1000*z+x) ]
   
   {- Prefixes for large 0-999. -}
   prefix :: Integral a => Parser a
   prefix = let
      prefix0 = options $ zip (table !! 0) [0..9]
      prefix2 = options $ zip (table !! 2) [10,20..90]
      prefix3 = options $ zip (table !! 3) [100,200..900]
      combine = prefix2 <&> prefix3 ~> uncurry (+)
      in prefix0
      <|> (prefix1 <&> (prefix2 <|> prefix3 <|> combine) ~> uncurry (+))
   
   {- Unit prefixes for large 10-999. The units part is more involved, hence this function. -}
   prefix1 :: Integral a => Parser a
   prefix1 = let
      abc  = ['a'..'z']
      no36 = abc \\ "coqtv"
      no79 = abc \\ "cdqstov"
      in parallel
         [ accept 0
         , match "un"                                 ~> const 1
         , match "duo"                                ~> const 2
         , match "tres"   & lookAhead (oneOf "coqtv") ~> const 3
         , match "tre"    & lookAhead (oneOf no36)    ~> const 3
         , match "quattuor"                           ~> const 4
         , match "quin"                               ~> const 5
         , match "sex"    & lookAhead (oneOf "co")    ~> const 6
         , match "ses"    & lookAhead (oneOf "qtv")   ~> const 6
         , match "se"     & lookAhead (oneOf no36)    ~> const 6
         , match "septen" & lookAhead (oneOf "cdqst") ~> const 7
         , match "septem" & lookAhead (oneOf "ov")    ~> const 7
         , match "septe"  & lookAhead (oneOf no79)    ~> const 7
         , match "octo"                               ~> const 8
         , match "noven"  & lookAhead (oneOf "cdqst") ~> const 9
         , match "novem"  & lookAhead (oneOf "ov")    ~> const 9
         , match "nove"   & lookAhead (oneOf no79)    ~> const 9 ]
   
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