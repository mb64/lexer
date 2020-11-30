module Interval where

import Prelude hiding (lookup)
import Control.Exception (assert)
import Control.Monad
import Data.Functor
import Data.IntMap (IntMap)
import qualified Data.IntMap.Lazy as IMap

-- import Debug.Trace

-- Keyed by start, stores end for each
newtype Map v = IM (IntMap (Int, v))
              deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

empty :: Map v
empty = IM IMap.empty

singleton :: (Int,Int) -> v -> Map v
singleton (s,e) v = IM $ IMap.singleton s (e,v)

lookup :: Int -> Map v -> Maybe v
lookup k (IM m) = do
  (_, (end,value)) <- IMap.lookupLE k m
  guard (k <= end)
  pure value

toList :: Map v -> [(Int,Int, v)]
toList (IM m) = IMap.toList m <&> \(s,(e,v)) -> (s,e,v)

merge :: (v -> v -> v) -> [(Int,Int,v)] -> [(Int,Int,v)] -> [(Int,Int,v)]
merge _ list [] = list
merge _ [] list = list
merge (<+>) ((s,e,v):rest) ((s',e',v'):rest')
  | (s,e) > (s',e') = merge (<+>) ((s',e',v'):rest') ((s,e,v):rest)
  | (s,e) == (s',e') = (s, e, v <+> v') : merge (<+>) rest rest'
  | s == s' = let a = (s, e, v <+> v')
                  b = (e+1, e', v')
              in a : merge (<+>) rest (b:rest')
  | e == e' = let a = (s, s'-1, v)
                  b = (s', e, v <+> v')
              in a : b : merge (<+>) rest rest'
  | e < s' = (s,e,v) : merge (<+>) rest ((s',e',v'):rest')
  | e < e' = let a = (s, s'-1, v)
                 b = (s', e, v <+> v')
                 c = (e+1, e', v')
             in a : b : merge (<+>) rest (c:rest')
  | e' < e = let a = (s, s'-1, v)
                 b = (s', e', v <+> v')
                 c = (e'+1, e, v)
             in a : b : merge (<+>) (c:rest) rest'
  | otherwise = error "cases are exhaustive"

-- | Needs to be ascending and disjoint
fromGoodList :: Eq v => [(Int,Int,v)] -> Map v
fromGoodList = IM . IMap.fromAscListWith noDups . map (\(s,e,v) -> (s,(e,v))) . joinAdj
  where noDups _ _ = error "internal error: duplicates should already be handled"
        joinAdj [] = []
        joinAdj [x] = [x]
        joinAdj ((s,e,v):(s',e',v'):rest)
          | e + 1 == e' && v == v' = joinAdj ((s,e',v):rest)
          | otherwise = (s,e,v):joinAdj ((s',e',v'):rest)

insertWith :: Eq v => (v -> v -> v) -> (Int,Int) -> v -> Map v -> Map v
insertWith (<+>) (s,e) v m = assert (s <= e)
    $ fromGoodList
    $ merge (<+>) [(s,e,v)]
    $ toList m

fromListWith :: Eq v => (v -> v -> v) -> [(Int,Int, v)] -> Map v
fromListWith (<+>) = unions (<+>) . map (\(s,e,v) -> singleton (s,e) v)

unionWith :: Eq v => (v -> v -> v) -> Map v -> Map v -> Map v
unionWith (<+>) m m' = fromGoodList $ merge (<+>) (toList m) (toList m')

unions :: (Eq v, Foldable t) => (v -> v -> v) -> t (Map v) -> Map v
unions (<+>) = fromGoodList . foldr go []
  where go m acc = merge (<+>) (toList m) acc
