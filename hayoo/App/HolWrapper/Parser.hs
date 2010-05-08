module App.HolWrapper.Parser
  ( customParser
  , defaultParser
  , replace
  ) where

--import App.HolWrapper.Common
import App.HolWrapper.Types

import Holumbus.Query.Language.Parser (parseQuery)
import Holumbus.Query.Language.Grammar
import Holumbus.Utility

import Text.ParserCombinators.Parsec

import qualified Data.Map as M
import Data.Char (isSpace)

-- | Parse a Query as you want
-- / (to be customized, i.e. customize all private functions)
customParser :: QueryParser -- QuerySettings -> Either String Query
customParser qs = let s = searchString qs
                  in caseSense $ customParseQuery $ addr "module" $ addr "package" s
-- . addCaseSensitive
                  where addr :: String -> String -> String
                        addr r s = s ++ (concat $ map (rCTC r) $ modules qs)
                        caseSense (Left s) = Left s
                        caseSense (Right q) | caseSensitive qs = Right $ toCase q
                                            | otherwise = Right q

-- | Use the default Holumbus parser for queries 
defaultParser :: QueryParser -- QuerySettings -> Either String Query
defaultParser = parseQuery . searchString

-- ## Private
toCase :: Query -> Query
toCase (Word s) = CaseWord s
toCase (Phrase s) = CasePhrase s
toCase (FuzzyWord s) = CaseWord s
toCase (BinQuery o q1 q2) = BinQuery o (toCase q1) (toCase q2)
toCase q = q

rCTC :: String -> RConfig -> String
rCTC s (Rank (n,_)) = ' ' : s ++ ":" ++ n
rCTC s (Name n) = ' ' : s ++ ":" ++ n

-- ----------------------------------------------------------------------------
{- |
  The following functions are from Timo B. Huebel (tbh@holumbus.org)
  (Copyright (C) 2008)

  The parser for the Hayoo! web search.
-}
-- ----------------------------------------------------------------------------

-- | Parse a query using the special Hayoo! syntax.
customParseQuery :: String -> Either String Query
customParseQuery = result . (parse query "")
  where
  result (Left err) = Left (show err)
  result (Right q)  = Right q

-- | A query may always be surrounded by whitespace
query :: Parser Query
query = spaces >> ((try sigQuery) <|> (andQuery))

-- | Parse an and query.
andQuery :: Parser Query
andQuery = do t <- orQuery
              try (andOp' t) <|> return t
  where
  andOp' r = do andOp
                q <- (notQuery <|> andQuery)
                return (BinQuery And r q)

-- | Parse an or query.
orQuery :: Parser Query
orQuery = do t <- contextQuery
             do orOp
                q <- orQuery
                return (BinQuery Or t q)
                <|> return t

-- | Parse a negation.
notQuery :: Parser Query
notQuery = do notOp
              q <- contextQuery
              return (Negation q)

-- | Parse a context query.
contextQuery :: Parser Query
contextQuery = try contextQuery' <|> parQuery
  where
  contextQuery' = do c <- contexts
                     spaces
                     _ <- char ':'
                     spaces
                     t <- parQuery
                     return (Specifier c t)

-- | Parse a query surrounded by parentheses.
parQuery :: Parser Query
parQuery = parQuery' <|> phraseQuery <|> wordQuery
  where
  parQuery' = do _ <- char '('
                 spaces
                 q <- andQuery
                 spaces
                 _ <- char ')'
                 return q

-- | Parse a phrase query.
phraseQuery :: Parser Query
phraseQuery = do p <- phrase
                 return (Phrase p)

-- | Parse a word query.
wordQuery :: Parser Query
wordQuery = do w <- word
               return (FuzzyWord w)

-- | Parse a signature.
sigQuery :: Parser Query
sigQuery = do
           r <- contains "->"
           s <- return (stripSignature r)
           n <- return (normalizeSignature r)
           return $ BinQuery Or (Specifier ["signature"] (Word s)) (Specifier ["normalized"] (Word n))

contains :: String -> Parser String
contains s = do
             pr <- many1 (noneOf s)
             _ <- string s
             po <- many1 anyChar
             return (pr ++ s ++ po)

-- | Parse an and operator.
andOp :: Parser ()
andOp = (try andOp') <|> spaces1
  where
  andOp' = do spaces
              _ <- string "AND" 
              spaces1
              return ()

-- | Parse an or operator.
orOp :: Parser ()
orOp = try orOp'
  where
  orOp' = do spaces
             _ <- string "OR"
             spaces1
             return ()

-- | Parse a not operator.
notOp :: Parser ()
notOp = try notOp'
  where
  notOp' = do spaces
              _ <- string "NOT" 
              spaces1
              return ()

-- | Parse a word.
word :: Parser String
word = many1 wordChar

-- | Parse a phrase.
phrase :: Parser String
phrase = do _ <- char '"'
            p <- many1 phraseChar
            _ <- char '"'
            return p

-- | Parse a character of a word.
wordChar :: Parser Char
wordChar = noneOf "\")( "

-- | Parse a character of a phrases.
phraseChar :: Parser Char
phraseChar = noneOf "\""

-- | Parse a list of contexts.
contexts :: Parser [String]
contexts = context `sepBy1` (char ',')

-- | Parse a context.
context :: Parser String
context = do spaces 
             c <- (many1 alphaNum)
             spaces
             return c

-- | Parse at least on white space character.
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- ## Signature manipulation

-- | Normalizes a Haskell signature, e.g. @String -> Int -> Int@ will be transformed to 
-- @a->b->b@. All whitespace will be removed from the resulting string.
normalizeSignature :: String -> String
normalizeSignature = join "->" . (replaceTypes M.empty ['a'..'z']) . split "->" . filter (not . isSpace)
  where
  replaceTypes _ _ [] = []
  replaceTypes v t (x:xs) = let (nv, ut, rx) = replace' in rx:(replaceTypes nv ut xs)
    where
    replace' = let ut = [head t] in maybe (M.insert r ut v, tail t, ut) (\n -> (v, t, n)) (M.lookup r v)
      where r = stripWith (\c -> (c == '(') || (c == ')')) x

-- | Strip unneeded whitespace from a signature, e.g. @String -> Map k a -> Int@ will be transformed
-- to @String->Map k a->Int@.
stripSignature :: String -> String
stripSignature = sep "->" . lsep "(" . rsep ")" . sep "." . sep "=>"
  where
  sep s = join s . map strip . split s
  lsep s = join s . map stripl . split s
  rsep s = join s . map stripr . split s


-- | Replace a given list with another list in a list.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = join new . split old $ l

