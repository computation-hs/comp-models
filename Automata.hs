{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Automata (
  Automata(..)
  ,State
  ,String
  ,DFA
  ,NFA
  ,process
  ,execute
  ,dfa
  ,nfa
) where

import Control.Monad.Writer
import Data.Maybe
import Data.List
import Convergence

{- Automata class -}
class (Monad m) => Automata a m | a -> m, m -> a where
  -- | 'initial' returns the initial(s) state(s).
  initial :: a -> m State
  -- | 'isFinal' returns the final predicate.
  isFinal :: a -> (State -> Bool)
  -- | 'delta' returns the delta function.
  delta   :: a -> (State -> Alpha -> m State)
  -- | 'accept' checks if a word is accepted.
  accept  :: a -> [Alpha] -> Bool
  -- | 'alpha' returns the alphabet
  alpha   :: a -> [Alpha]
  -- | 'toNFA' converts an automaton to a NFA.
  toNFA   :: a -> NFA
  -- | 'states' returns the set of states.
  states  :: a -> [State]
  states = listStates . toNFA

instance Automata DFA Maybe where
  initial             = initialDFA
  isFinal             = isFinalDFA
  delta               = deltaDFA
  alpha               = alphaDFA
  accept dfa          = maybe False (isFinal dfa) . process dfa
  toNFA (DFA i f t a) = NFA (maybeToList i) f (\s c -> maybeToList $ t s c) a

instance Automata NFA [] where
  initial     = initialNFA
  isFinal     = isFinalNFA
  delta       = deltaNFA
  alpha       = alphaNFA
  accept  nfa = any (isFinal nfa) . process nfa
  toNFA       = id


{- Basic types for automata -}

-- | 'State' is a type synonym of String for automata states.
type State = String

-- | 'Alpha' is a type synonym of String for automata symbols.
type Alpha = Char

-- | 'DFA' is the type for deterministic finite automata. Its constructor
--   takes the initial state, a predicate which defines final states and the
--   transition function. The transition function operates inside the Maybe
--   monad for error handling.
data DFA = DFA {
    initialDFA :: Maybe State,
    isFinalDFA :: State -> Bool,
    deltaDFA   :: State -> Alpha -> Maybe State,
    alphaDFA   :: [Alpha]
    }

-- | 'NFA' is the type for non-deterministic finite automata. It has one
--   constructor which takes the set of initial states, a predicate which
--   defines final states and the transition function. The transition
--   function operates inside the List Monad for non-determinism.
data NFA = NFA {
     initialNFA :: [State],
     isFinalNFA ::State -> Bool,
     deltaNFA   ::State -> Alpha -> [State],
     alphaNFA   :: [Alpha]
     }


{- Auxiliary functions -}

-- | 'addLog' creates a list of old states. Useful in logging functions.
addLog :: [State] -> State -> (State, [State])
addLog old new = (new, old)

-- | 'addFinal' adds final state to the list.
addFinal :: (State, [State]) -> [State]
addFinal (s,ss) = ss ++ [s]

-- | 'foldI' is equivalent to 'foldM' with its initial value inside a monad.
foldI :: (Monad m) => (a -> b -> m a) -> m a -> [b] -> m a
foldI f i xs = i >>= \x -> foldM f x xs

-- | 'initialW' returns initial state(s) inside minimum WriterT context.
initialW :: Automata a m => a -> WriterT [State] m State
initialW fa = WriterT $ liftM (addLog []) (initial fa)

-- | 'epsilon' is an auxiliary function used to calculate the epsilon closure.
epsilon :: NFA -> State -> [State]
epsilon fa s = delta fa s 'ɛ'

-- | 'step' is an auxiliary function used to calculate the epsilon closure.
step :: NFA -> ([State], [State]) -> ([State], [State])
step fa (old,acc) =
  let new = filter (flip notElem acc) (concatMap (epsilon fa) old)
  in (new, acc ++ new)

-- | 'closure' calculates a state epsilon closure.
closure :: NFA -> State -> [State]
closure fa s = nub $ snd $ until (null . fst) (step fa) ([s],[s])

-- | 'listStates' returns the set of states of a NFA.
listStates :: NFA -> [State]
listStates nfa = stabilize (nub . concat . (map (newStates nfa))) (initial nfa)

-- | 'newStates' is an auxiliary function used to calculate the set of states.
newStates :: NFA -> State -> [State]
newStates nfa s = s : concat [(delta nfa) s a | a <- (alpha nfa)]


{- Logging and processing functions -}

-- | 'process' returns the final state of a finite automaton given a word.
--   Returns a default value if execution wasn't correct.
process :: (Automata a m) => a -> [Alpha] -> m State
process fa = foldI (delta fa) (initial fa)

-- | 'logt' takes an automaton and returns its  delta function inside the
--   WriterT transformer. This allows keeping a log of states.
logt :: (Automata a m) => a -> (State -> Alpha -> WriterT [State] m State)
logt fa old symbol = WriterT $ liftM (addLog [old]) (delta fa old symbol)

-- | 'execute' returns the execution(s) log(s) given a word.
execute :: (Automata a m) => a -> [Alpha] -> m [State]
execute fa = liftM addFinal . runWriterT . foldI (logt fa) (initialW fa)


{- Example -}

t "1" 'A' = Just "2"
t "2" 'A' = Just "3"
t "2" 'B' = Just "1"
t "2" 'C' = Just "3"
t "3" 'D' = Just "5"
t "4" 'A' = Just "2"
t  _   _  = Nothing

a = ['A','B','C','D']
i = Just "1"
f = (`elem` ["4","5"])
dfa = DFA i f t a

t' "1" 'A' = ["2","4"]
t' "2" 'A' = ["3"]
t' "2" 'B' = ["1"]
t' "2" 'C' = ["3"]
t' "3" 'D' = ["5"]
t' "4" 'A' = ["2"]
t' _   _   = []

i' = ["1", "2"]
nfa = NFA i' f t' a

main = (print $ execute dfa "AAD") >> (print $ execute nfa "AA")
