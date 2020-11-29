module DFA where

import NFA

-- import Control.Monad.State
import Data.Monoid
import Data.List
-- import Data.Word
import Data.Maybe
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap.Lazy as IMap
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Interned.IntSet as ISet

-- Interned, hash-consed int sets
-- who knew this would be a useful thing to have
type DState = ISet.IntSet {- NState -}

type Transitions t = HMap.HashMap DState (IntMap {- Byte -} (Maybe t, DState))

data DFA t = DFA
  { startState  :: DState
  , transitions :: Transitions t
  } deriving (Show, Functor)

-- | Semigroup is for precedence if two rules happen to match at the same time
nfaToDfa :: forall t. Semigroup t => NFA t -> DFA t
nfaToDfa (NFA start _ nfaTrans) = DFA startState $ addDState startState HMap.empty
  where epsilons :: IntMap {- NState -} DState
        epsilons = flip IMap.mapWithKey nfaTrans \me tr ->
          ISet.unions $ ISet.singleton me : flip mapMaybe tr \case
            -- `Nothing` means transition of ε
            (Nothing, _, s) -> Just (epsilons IMap.! s)
            _ -> Nothing
        
        addDState :: DState -> Transitions t -> Transitions t
        addDState ds trans
          | ds `HMap.member` trans = trans
          | otherwise =
            let thisTrans :: IntMap {- Byte -} (Maybe t, DState)
                thisTrans = IMap.map (\(t, es) -> (t, ISet.unions es))
                  $ IMap.fromListWith (<>)
                  $ mapMaybe \case
                      -- we already handled the ε transitions, only take the real ones
                      (Just b, t, s) -> Just (fromIntegral b, (t, [epsilons IMap.! s]))
                      _ -> Nothing
                  $ concatMap (nfaTrans IMap.!)
                  $ ISet.toList ds
                trans' = HMap.insert ds thisTrans trans
            in foldl' (flip addDState) trans' $ map snd $ IMap.elems thisTrans

        startState = epsilons IMap.! start
