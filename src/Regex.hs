module Regex (regexParser) where

import Control.Exception (assert)
import Text.ParserCombinators.ReadP hiding (option)
import Data.Functor
import Control.Applicative hiding (many)

import NFA

regexParser :: ReadP (DiffNFA t)
regexParser = sepBy1 (allOf <$> many term) (char '|') <&> \case
  [] -> error "internal error: shouldn't be empty"
  [x] -> x
  xs -> anyOf xs

term :: ReadP (DiffNFA t)
term = do
  t <- simpleTerm
  mods <- many modifier <&> foldr (.) id
  pure $ mods t

modifier :: ReadP (DiffNFA t -> DiffNFA t)
modifier = char '?' $> option
       <|> char '*' $> star
       <|> char '+' $> plus

simpleTerm :: ReadP (DiffNFA t)
simpleTerm = char '(' *> regexParser <* char ')'
   <|> fmap byteRanges group
   <|> fmap (\x -> byteRanges [(x,x)]) normalChar

rangeMinus :: (Byte,Byte) -> (Byte,Byte) -> [(Byte,Byte)]
rangeMinus (a,b) (c,d)
  | b < c = [(a,b)]
  | d < a = [(a,b)]
  | a < c && b <= d = [(a,c-1)]
  | a < c && d < b = [(a,c-1),(d+1,b)]
  | c <= a && b <= d = []
  | c <= a && d < b = [(d+1,b)]
  | otherwise = error "bad"

complement :: [(Byte,Byte)] -> [(Byte,Byte)]
complement = foldr (\r -> concatMap (`rangeMinus` r)) [(minBound,maxBound)]

group :: ReadP [(Byte,Byte)]
group = char '[' *> many range <* char ']'
    <|> string "[^" *> (complement <$> many range) <* char ']'
  where range = (do
          a <- rangeChar
          _ <- char '-'
          b <- rangeChar
          assert (a <= b) $ pure ()
          pure (a,b)) <|> (rangeChar <&> \x -> (x,x))

normalChar, rangeChar :: ReadP Byte
normalChar = toByte <$> escaped "[]*+?|()\\"
rangeChar = toByte <$> escaped "^-[]*+?|()\\"

escaped :: String -> ReadP Char
escaped special = satisfy (`notElem` special)
  <|> char '\\' *> satisfy (`elem` special)
