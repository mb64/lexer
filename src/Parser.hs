-- should really be using text and a legit parser

module Parser (Token(..), parseFile) where

import Text.ParserCombinators.ReadP
import Data.List
import Data.Char
import Control.Monad.Fix

import Regex
import NFA

data Token = Token { tokPriority :: Int
                   , tokName :: Maybe String
                   } deriving (Eq, Ord)

instance Show Token where
  show (Token p n) = "[" ++ show p ++ "] " ++ show n

instance Semigroup Token where
  t1@(Token p1 _) <> t2@(Token p2 _) = if p1 <= p2 then t1 else t2

parseFile :: String -> Either [String] (NFA Token)
parseFile contents = mfix \nfa -> parseFile' (startState nfa) contents

parseFile' :: Int -> String -> Either [String] (NFA Token)
parseFile' start = \case
        ([], nfa) -> Right (anyOf nfa emptyNFA)
        (errs, _) -> Left errs
    . foldMap (\(n,l) -> case oneLine start n l of
        Right nfa -> ([],[nfa])
        Left e -> ([e],[]))
    . filter (not . comment . snd)
    . zip [1..]
    . map (dropWhileEnd isSpace . dropWhile isSpace)
    . lines

oneLine :: Int -> Int -> String -> Either String (DiffNFA Token)
oneLine start n line = if not goodName
  then Left $ "Line "++show n++": bad token name "++name
  else case parsedRegex of
    Nothing -> Left $ "Line "++show n++": parse error in regex"
    Just (nfa,_) -> if name == "--"
                    then Right $ nfa . skipTo start (Token n Nothing)
                    else Right $ nfa . token (Token n $ Just name)
  where (name,regex) = break isSpace line
        identChar c = c == '_' || isAlphaNum c
        goodName = name == "--" || all identChar name
        parsedRegex = find (null . snd)
            $ readP_to_S regexParser
            $ dropWhile isSpace regex

comment :: String -> Bool
comment s = s == "" || "//" `isPrefixOf` s
