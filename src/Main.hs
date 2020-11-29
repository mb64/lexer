{-# LANGUAGE QuasiQuotes #-}

module Main where

import NFA
import DFA

import Data.String.Interpolate (i)
import Data.Char
import Data.List
import qualified Data.IntMap.Lazy as IMap
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Interned.IntSet as ISet

comment :: DiffNFA String
comment = allOf
    [ char '/'
    , char '*'
    , star (anyOf [char 'a', char 'b', char 'c', char '/', allOf [char '*', anyOf (map char "abc*")]])
    , char '*'
    , char '/'
    , token "COMMENT"
    ]
ident :: DiffNFA String
ident = plus (anyOf $ map char "abc") . token "IDENT"
nfa :: NFA String
nfa = anyOf
    [ comment
    , ident
    , allOf [char '/', token "DIV"]
    , allOf [char '*', token "MUL"]
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
