module NFA where

-- import qualified Data.Interned.IntSet
import Control.Exception (assert)
import Data.Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap
import Data.List
import qualified Data.Map
import Data.Maybe
import Data.Word

type Byte = Word8
type NState = Int

data NFA t = NFA
  { startState  :: NState
  , endState    :: NState
  , transitions :: IntMap {- NState -} [(Maybe Byte, Maybe t, NState)]
  } deriving (Show, Functor)

toByte :: Char -> Byte
toByte c = if ord c < 256
           then fromIntegral (ord c)
           else error ("not a byte: " ++ show c)

-- Matches the empty string
emptyNFA :: NFA t
emptyNFA = NFA 0 0 $ IMap.fromList [(0, [])]

-- | pre-composes itself with its argument
-- Convert DiffNFA to NFA by passing it `empty`
type DiffNFA t = NFA t -> NFA t

-- Standard regex combinators

-- | `byte b` matches the single byte `b`
byte :: Byte -> DiffNFA t
byte b (NFA start end trans) = NFA start' end trans'
  where start' = (1+) $ fst $ fromJust $ IMap.lookupMax trans
        trans' = IMap.insert start' [(Just b, Nothing, start)] trans

-- | `char c` matches the single (byte) char `c`
-- char == byte . toByte
char :: Char -> DiffNFA t
char = byte . toByte

allOf :: [DiffNFA t] -> DiffNFA t
allOf = foldl' (.) id

-- | `anyOf xs` matches iff any of `xs` matches
anyOf :: [DiffNFA t] -> DiffNFA t
anyOf xs next@(NFA start end _) = NFA start'' end trans''
  where go (ss, nfa) a =
          let (NFA start' end' trans) = a nfa
          in assert (end' == end) (start':ss, NFA start end trans)
        (starts, NFA _ _ trans') = foldl' go ([], next) xs

        -- Connect all the starts together in a new start node
        start'' = (1+) $ fst $ fromJust $ IMap.lookupMax trans'
        trans'' = IMap.insert start'' [(Nothing, Nothing, start) | start <- starts] trans'

-- | `choice a b == anyOf [a,b]` matches either a or b
-- a|b
choice :: DiffNFA t -> DiffNFA t -> DiffNFA t
choice a b = anyOf [a,b]

-- | kleen star: zero or more copies
-- regex*
star :: DiffNFA t -> DiffNFA t
star a next = assert (end == endState next) $ NFA start end trans'
  where NFA start end trans = a next
        -- connect the end of a (start of next) to the start
        endOfA = startState next
        trans' = IMap.adjust ((Nothing, Nothing, start):) endOfA trans

-- | one or more copies
-- regex+ == regex regex*
plus :: DiffNFA t -> DiffNFA t
plus a = a . (star a)

-- | Zero or one copies
-- regex? == ε | regex
option :: DiffNFA t -> DiffNFA t
option a = choice id a