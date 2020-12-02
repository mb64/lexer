module DFA where

import NFA
import qualified Interval as I

import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap.Lazy as IMap
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Interned.IntSet as ISet

-- import Debug.Trace

-- Interned, hash-consed int sets
-- who knew this would be a useful thing to have
type DState = ISet.IntSet {- NState -}

type Transitions t = HMap.HashMap DState (I.Map {- Byte range -} (Maybe t, DState))

-- | No need to store an end state, since it's managed by storing tokens
data DFA t = DFA
  { startState  :: DState
  , transitions :: Transitions t
  } deriving (Show, Functor)

-- TODO remove Show
-- | Semigroup is for precedence if two rules happen to match at the same time
nfaToDfa :: forall t. (Show t,Semigroup t,Eq t) => NFA t -> DFA t
nfaToDfa (NFA start _ nfaTrans) = DFA dstart $ addDState dstart HMap.empty
  where -- the subtree of epsilon-connected states, plus any tokens that are found on it
        epsilons :: IntMap {- NState -} (Maybe t, DState)
        epsilons = flip IMap.mapWithKey nfaTrans \me tr ->
          second (ISet.unions . (ISet.singleton me :)) $ flip foldMap tr \case
            -- `Nothing` means transition of ε
            (Nothing, t, s) -> let (t', ds) = epsilons IMap.! s
                               in (t <> t', [ds])
            _ -> (Nothing, []) -- mempty

        addDState :: DState -> Transitions t -> Transitions t
        addDState ds trans
          | ds `HMap.member` trans = trans
          | otherwise =
            let thisTrans :: I.Map {- Byte range -} (Maybe t, DState)
                thisTrans = fmap (second ISet.unions)
                  $ I.fromListWith (<>)
                  $ mapMaybe \case
                      -- we already handled the ε transitions, only take the real ones
                      (Just (a,b), t, s) -> let (t', ds') = epsilons IMap.! s
                                        in Just (fromIntegral a, fromIntegral b, (t <> t', [ds']))
                      _ -> Nothing
                  $ concatMap (nfaTrans IMap.!)
                  $ ISet.toList ds
                trans' = HMap.insert ds thisTrans trans
            in foldl' (flip addDState) trans' $ map snd $ toList thisTrans

        dstart = snd $ epsilons IMap.! start


type UnifyM = StateT (HMap.HashMap DState DState) Maybe

-- | Deref the state in the unification table
-- Union find without the union
deref :: DState -> HMap.HashMap DState DState -> DState
deref ds unifs | Just ds' <- HMap.lookup ds unifs = deref ds' unifs
               | otherwise = ds

unify :: Eq t => DState -> DState -> Transitions t -> UnifyM ()
unify a b trans = do
  a' <- gets $ deref a
  b' <- gets $ deref b
  if a' == b' then pure () else do
    let as = trans HMap.! a
        bs = trans HMap.! b

    let go [] [] = pure []
        go [] _ = lift Nothing
        go _ [] = lift Nothing
        go ((s,e,(t,v)):rest) ((s',e',(t',v')):rest') = do
            guard $ s == s' && e == e' && t == t'
            ((v,v'):) <$> go rest rest'

    vals <- go (I.toList as) (I.toList bs)

    -- optimistically assume they can be unified
    let ab = a' `ISet.union` b'
    unless (a' == ab) $ modify $ HMap.insert a' ab
    unless (b' == ab) $ modify $ HMap.insert b' ab

    forM_ vals \(va, vb) -> unify va vb trans

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = map (x,) xs ++ pairs xs

-- | Minimize the DFA
-- O(should be faster)
-- TODO: implement a legit O(n²) algorithm
-- https://en.wikipedia.org/wiki/DFA_minimization
minDfa :: forall t. Eq t => DFA t -> DFA t
minDfa (DFA start trans) = DFA newStart newTrans
  where allUnifs = foldl' go mempty (pairs $ HMap.keys trans)

        newStart = deref start allUnifs
        newTrans = addDState newStart HMap.empty

        addDState :: DState -> Transitions t -> Transitions t
        addDState ds trans' =
          let ds' = deref ds allUnifs
          in if ds' `HMap.member` trans' then trans' else
            let thisTrans :: I.Map {- Byte range -} (Maybe t, DState)
                thisTrans = fmap (second \s -> deref s allUnifs)
                  $ trans HMap.! ds
                trans'' = HMap.insert ds' thisTrans trans'
            in foldl' (flip addDState) trans''
                $ map snd
                $ toList (trans HMap.! ds)

        go :: HMap.HashMap DState DState
           -> (DState, DState)
           -> HMap.HashMap DState DState
        go unifs (a,b) = fromMaybe unifs $ execStateT (unify a b trans) unifs
