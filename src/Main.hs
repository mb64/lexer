module Main where

import DFA
import Parser
import Graphviz

import Data.Foldable
import Data.Text.Lazy.IO qualified as T

run :: FilePath -> IO ()
run fileName = do
  contents <- readFile fileName
  case parseFile contents of
    Right nfa -> do
      let dfa = nfaToDfa nfa
      -- T.putStrLn $ renderNfa nfa
      -- T.putStrLn $ renderDfa dfa
      T.putStrLn $ renderDfa $ minDfa dfa
    Left [e] -> do
      putStrLn $ "Error parsing " ++ fileName ++ ":"
      putStrLn e
    Left errs -> do
      putStrLn $ show (length errs) ++ " errors parsing " ++ fileName ++ ":"
      traverse_ putStrLn errs

main :: IO ()
main = run "grammar.txt"
