{-# LANGUAGE QuasiQuotes #-}

module Main where

import NFA
import DFA
import Regex
import qualified Interval as I

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
    [ regex "/\\*([^\\*]|\\*+[^\\*/])*\\*+/"  . token "COMMENT"
    , regex "[a-zA-Z0-9][a-zA-Z0-9_]*"        . token "IDENT"
    , regex "/"                               . token "DIV"
    , regex "\\*"                             . token "MUL"
    ] emptyNFA

dfa :: DFA String
dfa = nfaToDfa nfa

label :: (Int,Int) -> String
label (s,e) | s == e = show (chr s)
            | otherwise = show (chr s) ++ "-" ++ show (chr e)

graphvis :: String
graphvis = unlines
    $ map (\case
        (s,(a,b,(Nothing,e))) ->
            [i|#{show $ show $ ISet.toList s} -> #{show $ show $ ISet.toList e} [label = "#{label (a,b)}"]|]
        (s,(a,b,(Just t,e))) ->
            [i|#{show $ show $ ISet.toList s} -> #{show $ show $ ISet.toList e} [label = "#{label (a,b)} (output #{t})"]|])
    $ concatMap (\(s,im) -> (s,) <$> I.toList im)
    $ HMap.toList (DFA.transitions $ minDfa dfa)

graphvisNfa :: String
graphvisNfa = unlines
    $ map (\case
        (s,(r,Nothing,e)) -> [i|    #{s} -> #{e} [label = "#{r}"]|]
        (s,(r,Just t,e))  -> [i|    #{s} -> #{e} [label = "#{r} (output #{t})"]|])
    $ concatMap sequence
    $ IMap.toList (NFA.transitions nfa)


main :: IO ()
main = putStrLn [i|
    digraph G {
        rankdir=LR;
        size="8,5";
        node [shape = circle];

        #{graphvis}
    } |]
