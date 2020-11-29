module Regex (regexParser) where

import Text.ParserCombinators.ReadP hiding (option)
import Data.Functor
import Control.Applicative hiding (many)

import NFA hiding (char)
import qualified NFA

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
   <|> fmap (anyOf . map NFA.char) group
   <|> fmap NFA.char normalChar

group :: ReadP [Char]
group = char '[' *> many normalChar <* char ']' -- TODO: negative group

normalChar :: ReadP Char
normalChar = let special = "[]*+?|()\\"
             in satisfy (`notElem` special)
             <|> char '\\' *> satisfy (`elem` special)
