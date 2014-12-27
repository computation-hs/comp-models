{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
import Control.Monad.Writer
import Data.Maybe

{- Automata class -}
class (Monad m) => Automata a m | a -> m, m -> a where
  -- | 'process' returns the final state of an automaton given a word.
  --   Returns default value if execution wasn't correct.
  process :: a -> [Alpha] -> m State
  -- | 'accept' checks if a word is accepted by an automaton.
  accept  :: a -> [Alpha] -> Bool
  -- | 'execute' Returns the execution(s) log(s) of all paths in an automaton
  --   given a word.
  execute :: a -> [Alpha] -> m [State]
  -- | 'log' takes a transition function and returns it inside the
  --   WriterT transformer. This allows keeping a log of the states.
  logt    :: (State -> Alpha -> m State) -> (State -> Alpha -> WriterT [State] m State)

instance Automata DFA Maybe where
  process     (DFA i _ t)     = foldM t i
  accept  dfa@(DFA _ f _)     = maybe False f . process dfa
  logt    t old symbol        = WriterT $ t old symbol >>= Just . addLog old
  execute     (DFA i _ t) ss  = (runWriterT $ foldM (logt t) i ss) >>= Just . addFinal

instance Automata NFA [] where
  process     (NFA i _ t) ss  = i >>= (\x -> foldM t x ss)
  accept  nfa@(NFA _ f _)     = any f . process nfa
  logt    t old symbol        = WriterT $ map (addLog old) (t old symbol)
  execute     (NFA i _ t) ss  = let i' = WriterT $ map (\x -> (x,[])) i  in
    map addFinal $ runWriterT $ i' >>= (\x -> foldM (logt t) x ss)


{- Basic types for automata -}

-- | 'State' is a type synonym of String for automata states.
type State = String

-- | 'Alpha' is a type synonym of String for automata symbols.
type Alpha = Char

-- | 'DFA' is the type for deterministic finite automata. Its constructor
--   takes the initial state, a predicate which defines final states and the
--   transition function. The transition function operates inside the Maybe
--   monad for error handling.
data DFA = DFA State (State -> Bool) (State -> Alpha -> Maybe State)

-- | 'NFA' is the type for non-deterministic finite automata. It has one
--   constructor which takes the set of initial states, a predicate which
--   defines final states and the transition function. The transition
--   function operates inside the List Monad for non-determinism.
data NFA = NFA [State] (State -> Bool) (State -> Alpha -> [State])


{- Example DFA -}

t :: State -> Alpha -> Maybe State
t "1" 'A' = Just "2"
t "2" 'A' = Just "3"
t "2" 'B' = Just "1"
t "2" 'C' = Just "3"
t "3" 'D' = Just "5"
t "4" 'A' = Just "2"
t  _   _  = Nothing

i :: State
i = "1"

f :: State -> Bool
f = (`elem` ["4","5"])

dfa = DFA i f t


{- Example NFA -}

t' :: State -> Alpha -> [State]
t' "1" 'A' = ["2","4"]
t' "2" 'A' = ["3"]
t' "2" 'B' = ["1"]
t' "2" 'C' = ["3"]
t' "3" 'D' = ["5"]
t' "4" 'A' = ["2"]
t' _   _   = []

i' :: [State]
i' = ["1", "2"]

nfa = NFA i' f t'


{- Auxiliary functions -}

-- | 'addLog' creates a list of old states. Used in logging functions.
addLog :: State -> State -> (State, [State])
addLog old new = (new, [old])

-- | 'addFinal' adds final state to a list. Useful when extracting
--   data from WriterT in an execution.
addFinal :: (State, [State]) -> [State]
addFinal (s,ss) = ss ++ [s]

{- Main -}

main = (print $ execute dfa "AAD") >> (print $ execute nfa "AA")
