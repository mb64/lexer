-- Output C code to lex using the DFA
module C (codegen) where

import Data.HashMap.Lazy qualified as HMap
import Data.Interned.IntSet qualified as ISet
import Language.C.Quote.GCC
import Language.C.Syntax

import DFA
import Parser (Token(..))
import qualified Interval as I

-- Quote with [cstm| ... ]
-- Unquote identifiers with $id:haskellId or $id:(haskell expression)
-- where haskellId, haskell expression :: String

-- TODO: Codegen typeclass, C instance, Zig instances

labelName :: DState -> String
labelName s = "state_" ++ show (ISet.identity s)

isEndState :: DFA t -> DState -> Bool
isEndState dfa s = null $ I.toList $ transitions dfa HMap.! s

-- Required headers:
-- stdint.h
-- stdbool.h
-- stdlib.h
codegen :: DFA Token -> [Definition]
codegen dfa = [cunit|
  typedef struct {
    typename size_t start;
    typename size_t end;
    int token_type;
  } LexResult;

  static LexResult next_token(const char *const text, typename size_t start, typename size_t const len) {
    typename bool has_fallback = false;
    typename bool fallback_is_skip = false;
    typename size_t fallback_end;
    int fallback_token;

    typename size_t i = start;

    goto $id:(labelName $ startState dfa);

    $stms:(concatMap (uncurry (state dfa)) $ HMap.toList $ transitions dfa)
  }
  |]

state :: DFA Token -> DState -> I.Map {- Byte range -} (Maybe Token, DState) -> [Stm]
state dfa s m = [cstms|
  $id:(labelName s):
    if (i >= len) {
      if (has_fallback) {
        if (fallback_is_skip) {
          has_fallback = false;
          start = fallback_end;
          goto $id:(labelName $ startState dfa);
        } else {
          return (typename LexResult) {
            .start = start,
            .end = fallback_end,
            .token_type = fallback_token,
          };
        }
      } else {
        return (typename LexResult) {
          .start = start,
          .end = i,
          .token_type = ERROR,
        };
      }
    }
    switch (text[i++]) {
      $stms:(concatMap (transition dfa) $ I.toList m)
      default:
        return (typename LexResult) {
          .start = start,
          .end = i,
          .token_type = ERROR,
        };
    }
  |]

transition :: DFA Token -> (Int, Int, (Maybe Token, DState)) -> [Stm]
transition dfa (s,e,(Just tok,next))
  | isEndState dfa next, Just name <- tokName tok = [cstms|
    case $int:s ... $int:e:
      return (typename TokenType) {
        .start = start,
        .end = i,
        .token_type = $id:name,
      };
    |]
  | isEndState dfa next, Nothing <- tokName tok = [cstms|
    case $int:s...$int:e:
      // Skip
      start = i;
      has_fallback = false;
      goto $id:(labelName $ startState dfa);
    |]
  | Just name <- tokName tok = [cstms|
    case $int:s ... $int:e:
      has_fallback = true;
      fallback_is_skip = false;
      fallback_end = i;
      fallback_token = $id:name;
      goto $id:(labelName next);
    |]
  | Nothing <- tokName tok = [cstms|
    case $int:s...$int:e:
      has_fallback = true;
      fallback_is_skip = true;
      fallback_end = i;
      goto $id:(labelName next);
    |]
transition _dfa (s,e,(Nothing,next)) = [cstms|
  case $int:s...$int:e:
    goto $id:(labelName next);
  |]
