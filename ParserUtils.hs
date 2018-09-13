module NumberName.ParserUtils where

import Data.List (nub, sort)

type Parser a = String -> [(a, String)]

{- Fully implement a Parser -}
parser :: Eq a => Parser a -> String -> a
parser p s = case nub . filter (null . snd) $ p s of 
   [x] -> fst x
   _   -> errorWithoutStackTrace "no parse"

{- Make changes to a string before parsing. -}
preprocess :: [(String, String)] -> String -> String
preprocess ts = aux ts where
   aux :: [(String, String)] -> String -> String
   aux [] []    = []
   aux [] (c:s) = c : aux ts s
   aux (f:fs) s = aux fs $ case match (fst f) s of
      [(_,r)] -> snd f ++ r
      [     ] -> s

{-- PARSER COMBINATORS --}

{- Distinct choice of Parsers -}
(<|>) :: Parser a -> Parser a -> Parser a
f <|> g = \s -> f s ++ g s

{- Sequence Parsers, keep second result -}
(&>) :: Parser a -> Parser b -> Parser b
f &> g = \s -> [ (x,q) | (_,r) <- f s, (x,q) <- g r ]

{- Convenient synonym -}
(&) :: Parser a -> Parser b -> Parser b
(&) = (&>)

{- Sequence Parsers, keep first result -}
(<&) :: Parser a -> Parser b -> Parser a
f <& g = \s -> [ (x,q) | (x,r) <- f s, (_,q) <- g r ]

{- Sequence Parsers, combine results -}
(<&>) :: Parser a -> Parser b -> Parser (a,b)
f <&> g = \s -> [ ((x,y),q) | (x,r) <- f s, (y,q) <- g r ]

{- Transform results of parsing -}
(~>) :: Parser a -> (a -> b) -> Parser b
f ~> t = \s -> [ (t x, r) | (x, r) <- f s ]

{- Transform results of parsing -}
(<~) :: (a -> b) -> Parser a -> Parser b
t <~ f = \s -> [ (t x, r) | (x, r) <- f s ]

{- One or more -}
many :: Parser a -> Parser a
many f = f <|> (f & many f)

{- Zero or more -}
some :: Parser a -> Parser a
some = optional . many

{- Zero or one. -}
optional ::  Parser a -> Parser a
optional f = nothing <|> f

{- Combines a collection of parsers disjunctively -}
parallel :: Foldable t => t (Parser a) -> Parser a
parallel = foldr (<|>) reject

{- Parses without consuming characters. -}
lookAhead :: Parser a -> Parser a
lookAhead f = \s -> [ (x,s) | (x,r) <- f s ]

{-- BASIC PARSERS --}

{- Fails on all input. -}
reject :: Parser a
reject = const []

{- Consumes no tokens, "parsing" the supplied value. -}
accept :: a -> Parser a
accept x = return . (,) x

-- {- Accepts a fixed number of any characters. -}
-- chars :: Int -> Parser String
-- chars n = return . splitAt n

{- Match a given string. -}
match :: String -> Parser String
match s = filter ((==) s . fst) . return . splitAt (length s)

{- Uses a lookup list as a Parser. -}
options :: [(String,a)] -> Parser a
options = foldr1 (<|>) . map (\(s,x) -> match s ~> const x)

-- {- Parse a list of items.
 -- - Arg 1: Parser for items.
 -- - Arg 2: Delimiter string. 
 -- -}
-- list :: Parser a -> String -> Parser [a]
-- list item delim = let
   -- emptyLst = accept []
   -- lstHead = item
   -- lstTail = emptyLst <|> (match delim & list item delim)
   -- in emptyLst <|> ((lstHead <&> lstTail) ~> uncurry (:))

{- OFTEN-USED PARSERS -}

{- Matches an empty string only. Make sure you discard this result! -}
nothing :: Parser a
nothing = accept undefined

{- Matches any character. -}
anyChar :: Parser Char
anyChar [   ] = []
anyChar (c:s) = [(c,s)]

{- Matches one of the given characters. -}
oneOf :: [Char] -> Parser Char
oneOf cs = parallel [match [c] ~> const c | c <- cs]

{- Match a space. -}
space :: Parser String
space = match " "

{- Match a block of contiguous whitespace. -}
spaces :: Parser String
spaces = many space
