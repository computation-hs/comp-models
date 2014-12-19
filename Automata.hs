import Control.Monad.Writer
import Data.Maybe


{- Basic types for automata -}

-- | 'State' is a type synonym of String for automata states.
type State = String
-- | 'Alpha' is a type synonym of String for automata symbols.
type Alpha = Char

-- | 'DFA' is the type for deterministic finite automata. It has one
--   constructor which takes the initial state, a predicate which
--   defines final states and the transition function. The transition
--   function operates always inside the Maybe Monad for error handling.
data DFA = DFA State (State -> Bool) (State -> Alpha -> Maybe State)

-- | 'NFA' is the type for non-deterministic finite automata. It has one
--   constructor which takes the initial state, a predicate which
--   defines final states and the transition function. The transition
--   function operates always inside the List Monad for non-determinism.
data NFA = NFA State (State -> Bool) (State -> Alpha -> [State])


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

nfa = NFA i f t'


{- Automata execution functions -}

-- | 'process' returns the final state of a 'DFA' given a word. Operates
--   inside the Maybe Monad. Returns the end state in case the execution
--   was correct and Nothing otherwise.
process :: DFA -> [Alpha] -> Maybe State
process (DFA i f t) = foldM t i

-- | 'accept' checks if a certain word is accepted by a given 'DFA'.
--   It uses the 'process' function for execution and the 'DFA' predicate
--   for checking whether is final.
accept :: DFA -> [Alpha] -> Bool
accept dfa@(DFA _ f _) = maybe False f . process dfa

{- Automata log functions -}

-- | 'addLog' creates the pair necessary for the WriterT monad. It creates
--   a list in which the old states are added. It is used in all logging
--   functions for automata.
addLog :: State -> State -> (State, [State])
addLog old new = (new, [old])

-- | 'addFinal' adds the final state to a list. This is useful when extracting
--   data from the WriterT monad in an automaton execution.
addFinal :: (State, [State]) -> [State]
addFinal (s,ss) = ss ++ [s]

-- | 'logDFA' takes a DFA transition function and returns this function
--   inside the WriterT monad transformer. This makes possible keeping a
--   log of the states the automaton has been in.
logDFA :: (State -> Alpha -> Maybe State) -> (State -> Alpha -> WriterT [State] Maybe State)
logDFA t old = WriterT . maybe Nothing (Just . addLog old) . t old

-- | 'logNFA' takes a NFA transition function and returns this function
--   inside the WriterT monad transformer. This makes possible keeping a
--   log of the states the automaton has been in, keeping a separate log
--   for each path.
logNFA :: (State -> Alpha -> [State]) -> (State -> Alpha -> WriterT [State] [] State)
logNFA t old symbol = WriterT $ map (addLog old) (t old symbol)

-- | 'executeDFA' Returns the execution log of a DFA given a word.
--   It uses the 'logDFA' auxiliary function and operates inside a
--   combined monad using both WriterT and Maybe types.
executeDFA :: DFA -> [Alpha] -> Maybe [State]
executeDFA (DFA i _ t) = (maybe Nothing (Just . addFinal)) . runWriterT . foldM (logDFA t) i

-- | 'executeNFA' Returns the executions logs of all paths in a NFA
--   given a word. It uses the 'logNFA' auxiliary function and
--   operates inside a combined monad using both WriterT and Maybe
--   types.
executeNFA :: NFA -> [Alpha] -> [[State]]
executeNFA (NFA i _ t) = map addFinal . runWriterT . foldM (logNFA t) i


{- Main -}

main = mapM_ putStrLn [ show $ executeDFA dfa "AAD", show $ executeNFA nfa "AA" ]
