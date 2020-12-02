-- | Utilities for rendering the finite automata with graphviz
module Graphviz (renderNfa, renderDfa) where

import NFA
import DFA
import Interval qualified as I

import Data.Char
import Data.Function
import Data.Foldable
import Data.Text.Lazy (Text, pack)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing
import Data.GraphViz.Types.Monadic
import Data.IntMap.Lazy qualified as IMap
import Data.HashMap.Lazy qualified as HMap
import Data.Interned.IntSet (IntSet)
import Data.Interned.IntSet qualified as ISet

renderNfa :: Show t => NFA t -> Text
renderNfa n = render (pack . show) (NFA.startState n) (nfa n)

renderDfa :: Show t => DFA t -> Text
renderDfa d = render (pack . show . ISet.toList) (DFA.startState d) (dfa d)

render :: (n -> Text) -> n -> Dot n -> Text
render pr start g = renderDot $ toDot $ fmap pr $ digraph' $ do
  graphAttrs [RankDir FromLeft]
  nodeAttrs [shape Circle]
  node start [shape MDiamond]
  g

-- Wish haskell had ML's >>
-- then it'd be NFA.transitions >> IMap.toList >> traverse_ \case ...
nfa :: Show t => NFA t -> Dot Int
nfa n = IMap.toList (NFA.transitions n) & concatMap sequence & traverse_ \case
  (s,(r,Nothing,e))
      -> edge s e [toLabel $ maybe "ε" label r]
  (s,(r,Just tk,e))
      -> edge s e [toLabel $ maybe "ε" label r ++ " (token " ++ show tk ++ ")"]

dfa :: Show t => DFA t -> Dot IntSet
dfa n = HMap.toList (DFA.transitions n)
  & concatMap (traverse I.toList)
  & traverse_ \case
      (s,(a,b,(Nothing,e)))
          -> edge s e [toLabel $ label (a,b)]
      (s,(a,b,(Just tk,e)))
          -> edge s e [toLabel $ label (a,b) ++ " (token " ++ show tk ++ ")"]

label :: Integral a => (a,a) -> String
label (s,e) | s == e = show (ch s)
            | otherwise = show (ch s) ++ "-" ++ show (ch e)
  where ch = chr . fromIntegral
