{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
import Control.Monad.Writer
import Data.Maybe

{- Automata class -}
class (Monad m) => Automata a m | a -> m, m -> a where
  -- | 'inital' returns the initial(s) state(s).
  initial :: a -> m State
  -- | 'isFinal' returns the final predicate.
  isFinal :: a -> (State -> Bool)
  -- | 'delta' returns the delta function.
  delta   :: a -> (State -> Alpha -> m State)
  -- | 'accept' checks if a word is accepted by an automaton.
  accept  :: a -> [Alpha] -> Bool

instance Automata DFA Maybe where
  initial (DFA i _ _) = i
  isFinal (DFA _ f _) = f
  delta   (DFA _ _ t) = t
  accept  dfa         = maybe False (isFinal nfa) . process dfa

instance Automata NFA [] where
  initial (NFA i _ _)  = i
  isFinal (NFA _ f _)  = f
  delta   (NFA _ _ t)  = t
  accept  nfa          = any (isFinal nfa) . process nfa


{- Basic types for automata -}

-- | 'State' is a type synonym of String for automata states.
type State = String

-- | 'Alpha' is a type synonym of String for automata symbols.
type Alpha = Char

-- | 'DFA' is the type for deterministic finite automata. Its constructor
--   takes the initial state, a predicate which defines final states and the
--   transition function. The transition function operates inside the Maybe
--   monad for error handling.
data DFA = DFA (Maybe State) (State -> Bool) (State -> Alpha -> Maybe State)

-- | 'NFA' is the type for non-deterministic finite automata. It has one
--   constructor which takes the set of initial states, a predicate which
--   defines final states and the transition function. The transition
--   function operates inside the List Monad for non-determinism.
data NFA = NFA [State] (State -> Bool) (State -> Alpha -> [State])


{- Auxiliary functions -}

-- | 'addLog' creates a list of old states. Used in logging functions.
addLog :: State -> State -> (State, [State])
addLog old new = (new, [old])

-- | 'addFinal' adds final state to a list. Useful when extracting
--   data from WriterT in an execution.
addFinal :: (State, [State]) -> [State]
addFinal (s,ss) = ss ++ [s]

-- | 'process' returns the final state of a finite automaton given a word.
--   Returns a default value if execution wasn't correct.
process :: (Automata a m) => a -> [Alpha] -> m State
process fa ss  = (initial fa) >>= (\x -> foldM (delta fa) x ss)

-- | 'logt' takes a delta function and returns it inside the
--   WriterT transformer. This allows keeping a log of the states.
logt :: (Automata a m) => a -> (State -> Alpha -> WriterT [State] m State)
logt fa old symbol = WriterT $ delta fa old symbol >>= return . addLog old

-- | 'initalW' returns the initial state(s) inside minimum WriterT context.
initialW :: Automata a m => a -> WriterT [a1] m State
initialW fa = WriterT $ (initial fa) >>= return . (\x -> (x,[]))

-- | 'execute' returns the execution(s) log(s) given a word.
execute :: (Automata a m) => a -> [Alpha] -> m [State]
execute fa ss  = (runWriterT $ initialW fa >>= (\x -> foldM (logt fa) x ss)) >>= return . addFinal

{- Example -}

t "1" 'A' = Just "2"
t "2" 'A' = Just "3"
t "2" 'B' = Just "1"
t "2" 'C' = Just "3"
t "3" 'D' = Just "5"
t "4" 'A' = Just "2"
t  _   _  = Nothing

i = Just "1"
f = (`elem` ["4","5"])
dfa = DFA i f t

t' "1" 'A' = ["2","4"]
t' "2" 'A' = ["3"]
t' "2" 'B' = ["1"]
t' "2" 'C' = ["3"]
t' "3" 'D' = ["5"]
t' "4" 'A' = ["2"]
t' _   _   = []

i' = ["1", "2"]
nfa = NFA i' f t'

main = (print $ execute dfa "AAD") >> (print $ execute nfa "AA")
