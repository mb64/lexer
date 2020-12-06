module Main where

import C
import DFA
import Graphviz
import NFA (NFA)
import Parser

import Data.Foldable
import Data.Text.Lazy.IO qualified as T
import System.Environment
import Text.PrettyPrint.Mainland.Class (pprint)

run :: (NFA Token -> IO ()) -> FilePath -> IO ()
run action fileName = do
  contents <- readFile fileName
  case parseFile contents of
    Right nfa -> action nfa
    Left [e] -> do
      putStrLn $ "Error parsing " ++ fileName ++ ":"
      putStrLn e
    Left errs -> do
      putStrLn $ show (length errs) ++ " errors parsing " ++ fileName ++ ":"
      traverse_ putStrLn errs

outputC, outputGv, outputGvAll :: NFA Token -> IO ()
outputC = pprint . codegen . minDfa . nfaToDfa
outputGv = T.putStrLn . renderDfa . minDfa . nfaToDfa
outputGvAll nfa = do
  let dfa = nfaToDfa nfa
  T.putStrLn $ renderNfa nfa
  T.putStrLn $ renderDfa dfa
  T.putStrLn $ renderDfa $ minDfa dfa

main :: IO ()
main = getArgs >>= \case
  [file] -> run outputC file
  ["-c", file] -> run outputC file
  ["-graphviz", file] -> run outputGv file
  ["-gv", file] -> run outputGv file
  ["-graphviz-all", file] -> run outputGvAll file
  ["-gv-all", file] -> run outputGvAll file
  _ -> do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " [command] FILENAME"
    putStrLn "  where command is one of:"
    putStrLn "    -c                      Output C code (default)"
    putStrLn "    -graphviz, -gv          Output the minimized DFA as graphviz"
    putStrLn "    -graphviz-all, -gv-all  Output all automata as graphviz"
