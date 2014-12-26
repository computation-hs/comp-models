{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad.Writer
import Data.Maybe

{- Automata class -}
class (Monad m) => Automata a m where
  accept  :: a -> [Alpha] -> Bool
  process :: a -> [Alpha] -> m State
  execute :: a -> [Alpha] -> m [State]

instance Automata DFA Maybe where
  accept  = acceptDFA
  process = processDFA
  execute = executeDFA

instance Automata NFA [] where
  accept  = acceptNFA
  process = processNFA
  execute = executeNFA

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


{- Automata execution functions -}

-- | 'processDFA' returns the final state of a 'DFA' given a word. Operates
--   inside the Maybe Monad. Returns the end state in case execution
--   was correct and Nothing otherwise.
processDFA :: DFA -> [Alpha] -> Maybe State
processDFA (DFA i _ t) = foldM t i

-- | 'acceptDFA' checks if a word is accepted by a 'DFA'. It uses 'processDFA'
--   function for execution and the 'DFA' predicate for checking.
acceptDFA :: DFA -> [Alpha] -> Bool
acceptDFA dfa@(DFA _ f _) = maybe False f . processDFA dfa

-- | 'processNFA' returns the final state of a 'NFA' given a word. Operates
--   inside the LIst Monad. Returns the list of end states.
processNFA :: NFA -> [Alpha] -> [State]
processNFA (NFA i _ t) ss = i >>= (\x -> foldM t x ss)

-- | 'acceptNFA' checks if a word is accepted by a 'NFA'. It uses 'processNFA'
--   function for execution and the 'NFA' predicate for checking.
acceptNFA :: NFA -> [Alpha] -> Bool
acceptNFA nfa@(NFA _ f _) = any f . processNFA nfa


{- Automata log functions -}

-- | 'addLog' creates a list in which the old states are added. It is used in
--   all logging functions for automata.
addLog :: State -> State -> (State, [State])
addLog old new = (new, [old])

-- | 'addFinal' adds the final state to a list. This is useful when extracting
--   data from the WriterT monad in an automaton execution.
addFinal :: (State, [State]) -> [State]
addFinal (s,ss) = ss ++ [s]


-- | 'logDFA' takes a DFA transition function and returns it inside the
--   WriterT monad transformer. This allows keeping a log of the states.
logDFA :: (State -> Alpha -> Maybe State) -> (State -> Alpha -> WriterT [State] Maybe State)
logDFA t old symbol = WriterT $ t old symbol >>= Just . addLog old

-- | 'executeDFA' Returns the execution log of a DFA given a word.
--   It operates inside a monad using both WriterT and Maybe types.
executeDFA :: DFA -> [Alpha] -> Maybe [State]
executeDFA (DFA i _ t) ss = (runWriterT $ foldM (logDFA t) i ss) >>= Just . addFinal


-- | 'logNFA' takes a NFA transition function and returns it inside WriterT
--   monad transformer. This allows keeping a log of states for each path.
logNFA :: (State -> Alpha -> [State]) -> (State -> Alpha -> WriterT [State] [] State)
logNFA t old symbol = WriterT $ map (addLog old) (t old symbol)

-- | 'executeNFA' Returns the executions logs of all paths in a NFA
--   given a word. It operates inside a monad using both WriterT and List types.
executeNFA :: NFA -> [Alpha] -> [[State]]
executeNFA (NFA i _ t) ss =
  let i' = WriterT $ map (\x -> (x,[])) i  in
    map addFinal $ runWriterT $ i' >>= (\x -> foldM (logNFA t) x ss)


{- Main -}

main = mapM_ putStrLn [ show $ executeDFA dfa "AAD", show $ executeNFA nfa "AA" ]
