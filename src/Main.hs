module Main where

import Prelude hiding (putStrLn)

import NFA
import DFA
import Regex
import Graphviz

import Data.List
import Data.Maybe
import Data.Text.Lazy.IO
import Text.ParserCombinators.ReadP (readP_to_S)

regex :: String -> DiffNFA t
regex = fst . fromJust . find (null . snd) . readP_to_S regexParser

nfa :: NFA String
nfa = anyOf
    [ regex "/\\*([^\\*]|\\*+[^\\*/])*\\*+/"  . token "SKIP"
    , regex "[a-zA-Z0-9][a-zA-Z0-9_]*"        . token "IDENT"
    , regex "/"                               . token "DIV"
    , regex "\\*"                             . token "MUL"
    , regex "//[^\n]*\n"                      . token "SKIP"
    , regex "[ \t\n]+"                        . token "SKIP"
    ] emptyNFA

dfa :: DFA String
dfa = minDfa $ nfaToDfa nfa

main :: IO ()
main = putStrLn $ renderDfa dfa
