-- This module provides functions to give full names to symbolic numbers.
-- Uses the Conway-Weschler naming scheme.
module NumberName.NameNum where

import AbLib.Data.List ((!?))
import AbLib.Control.Safe (safe)
import Data.Maybe (fromJust, listToMaybe, catMaybes)
import Control.Applicative (liftA2)

{-
   DIGIT := one | two | three | four | five | six | seven | eight | nine
   TEENS := ten | eleven | twelve | thirteen | fourteen | fifteen | sixteen | seventeen | eighteen | nineteen
   TENS  := twenty | thirty | forty | fifty | sixty | seventy | eighty | ninety

   D2 := DIGIT | TEENS | TENS | TENS DIGIT
   NT := and D2 | ""

   D3 := D2 | DIGIT hundred NT

   T0 := D3
   T1 := T0 | D3 thousand T0 | D3 thousand NT
   T2 := T1 | D3 million T1  | D3 million NT
   T3 := T2 | D3 billion T2  | D3 billion NT
   et cetera
-}

{- Gives the full name of an Integral value. -}
nameInt :: Integral a => a -> String
nameInt n = case compare n 0 of
   LT -> "negative" ++ nameInt (negate n)
   EQ -> "zero"
   GT -> fromJust $ build $ split n
   where

   {- Concatenates Maybe Strings. -}
   (<<) :: Maybe String -> Maybe String -> Maybe String
   (<<) = liftA2 (++)

   {- Return the first `Just` in a list. Nothing if there are none. -}
   first :: [Maybe a] -> Maybe a
   first = listToMaybe . catMaybes

   {- Splits a number into groups of 3 digits in descending order. -}
   split :: Integral a => a -> [a]
   split 0 = []
   split n = let
      (d,m) = divMod n 1000
      in (split d) ++ [m] 

   {- Names a grouped number. -}
   build :: Integral a => [a] -> Maybe String
   build (n:[]) = digit3 n                               -- Final three digits
   build (0:ns) = build ns                               -- Ignore leading zero groups
   build (n:ns) = let
      ord = length $ tail $ ns                           -- Order (large number) of current group
      cur = digit3 n << Just (" " ++ nameLarge ord)      -- Full name of current group
      in cur << first [term ns, Just ", " << build ns]   -- Check if only 2 digits left (requires "and")

   {- Names a 2-digit grouped number, assuming it is the final 2 digits of a larger number. -}
   term :: Integral a => [a] -> Maybe String
   term (0:[]) = Just ""                        -- All groups are zero
   term (n:[]) = Just " and " << digit2 n       -- Last group is nonzero
   term (0:ns) = term ns                        -- Ignore leading zero groups
   term _      = Nothing                        -- Empty lists or more than 2 digits

   {- Names a 3-digit number -}
   digit3 :: Integral a => a -> Maybe String
   digit3 n = let
      (d,m) = divMod n 100
      in first [digit2 n, digit d << Just " hundred" << term [m]]

   {- Names a 2-digit number -}
   digit2 :: Integral a => a -> Maybe String
   digit2 n = let
      m = mod n 10
      in first [digit n, teens n, tens n, tens (n - m) << Just " " << digit m]

   {- Names a strict multiple of 10 -}
   tens :: Integral a => a -> Maybe String
   tens = flip lookup
      [ (20,"twenty"), (30,"thirty"),  (40,"forty"),  (50,"fifty")
      , (60,"sixty"),  (70,"seventy"), (80,"eighty"), (90,"ninety") ]

   {- Names a teen number -}
   teens :: Integral a => a -> Maybe String
   teens x = lookup x
      [ (10,"ten"), (11,"eleven"), (12,"twelve"), (13,"thir"), (14,"four")
      , (15,"fif"), (16,"six"),    (17,"seven"),  (18,"eigh"), (19,"nine") ]
      << (Just $ if elem x [10..12] then "" else "teen") -- Perhaps excessive shortcutting

   {- Names a single digit number -}
   digit :: Integral a => a -> Maybe String
   digit = flip lookup
      [ (1,"one"),   (2,"two"),   (3,"three")
      , (4,"four"),  (5,"five"),  (6,"six")
      , (7,"seven"), (8,"eight"), (9,"nine") ]

{- Names 10^n given n -}
power10 :: Integral a => a -> String
power10 n | n < 0 = errorWithoutStackTrace "negative value"
power10 n = let
   (d,p) = divMod n 3
   prefix = nameInt (10 ^ p)
   illion = safe nameLarge (d - 1)
   in prefix ++ (maybe "" id $ fmap (' ':) illion)

{- Names 1000^(n+1) given n -}
nameLarge :: Integral a => a -> String
nameLarge n | n < 0 = errorWithoutStackTrace "negative value"
nameLarge 0 = "thousand"            -- Special case (see note below)
nameLarge n = nameLarge' n where    -- Direct call to auxiliary (see note below)
   {-
      Special case prevents 0 -> "nillion".
      Auxiliary prevents 1000 -> "millithousand", etc. (should be millinillion).
      If we allow 1000 to be called "one nillion", both workarounds can be removed.
      I vote in favour of this motion.
   -}

   {- Would be top-level definition but 1000 is "one nillion" -}
   nameLarge' :: Integral a => a -> String
   nameLarge' n = case prefix 0 n of
      "" | n < 1000 -> assemble $ build 1 n
      "" -> (init . init) (nameLarge' $ div n 1000) ++ (nameLarge' $ mod n 1000)
      pf -> pf ++ "illion"

   {- incrementally builds the prefix structure -}
   build :: Integral a => a -> a -> [String]
   build 4 _ = []
   build d n = (prefix d $ mod n 10) : (build (d+1) $ div n 10)

   {- Returns everything before the "illion". -}
   prefix :: Integral a => a -> a -> String
   prefix n k = maybe "" id $ table !? n' >>= (!? k')
      where
      n' = fromIntegral n :: Int
      k' = fromIntegral k :: Int
      table =
         [ [ "n", "m",       "b",            "tr"
           , "quadr",        "quint",        "sext"
           , "sept",         "oct",          "non"         ]
         , [ "", "un",       "duo",          "tre"
           , "quattuor",     "quin",         "se"
           , "septe",        "octo",         "nove"        ]
         , [ "", "deci",     "viginti",      "triginta"
           , "quadraginta",  "quinquaginta", "sexaginta"
           , "septuaginta",  "octoginta",    "nonaginta"   ]
         , [ "", "centi",    "ducenti",      "trecenti"
           , "quadringenti", "quingenti",    "sescenti"
           , "septingenti",  "octingenti",   "nongenti"    ] ]
   
   {- Puts all the elements together. -}
   assemble :: [String] -> String
   assemble [a,b,c] = assemble [a, b++c]
   assemble [a,b] = init (a ++ extra a (head b) ++ b) ++ "illion" where
      {- Extra Latin grammatical suffix rules. -}
      extra :: String -> Char -> String
      extra "se" c
         | any (c==) "qtv" = "s"
         | any (c==) "oc"  = "x"
      extra "tre" c
         | extra "se" c /= "" = "s"
      extra "nove" c
         | any (c==) "cdqst" = "n"
         | any (c==) "ov" = "m"
      extra "septe" c = extra "nove" c
      extra _ _ = ""
