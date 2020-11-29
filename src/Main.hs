{-# LANGUAGE QuasiQuotes #-}

module Main where

import NFA
import DFA
import Regex

import Data.String.Interpolate (i)
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.IntMap.Lazy as IMap
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Interned.IntSet as ISet
import Text.ParserCombinators.ReadP (readP_to_S)

regex :: String -> DiffNFA t
regex = fst . fromJust . find (null . snd) . readP_to_S regexParser

nfa :: NFA String
nfa = anyOf
    [ regex "/\\*([abc/]|\\**[abc])*\\*/"   . token "COMMENT"
    , regex "[abc]+"                        . token "IDENT"
    , regex "/"                             . token "DIV"
    , regex "\\*"                           . token "MUL"
    ] emptyNFA

dfa :: DFA String
dfa = minDfa $ nfaToDfa nfa

graphvis :: String
graphvis = unlines
    $ map (\case
        (s,(b,(Nothing,e))) ->
            [i|#{show $ show $ ISet.toList s} -> #{show $ show $ ISet.toList e} [label = "#{[chr b]}"]|]
        (s,(b,(Just t,e))) ->
            [i|#{show $ show $ ISet.toList s} -> #{show $ show $ ISet.toList e} [label = "#{[chr b]} (output #{t})"]|])
    $ concatMap (\(s,im) -> (s,) <$> IMap.toList im)
    $ HMap.toList (DFA.transitions dfa)

main :: IO ()
main = putStrLn [i|
    digraph G {
        rankdir=LR;
        size="8,5";
        node [shape = circle];

        #{graphvis}
    } |]
