import Control.Monad.Writer

--- Basic types for automata ---

type State = String -- ^ Type synonym for automata states.
type Alpha = Char   -- ^ Type synonym for automata symbols.

data DFA = DFA
    { initial :: State
    , isFinal :: State -> Bool
    , transition :: State -> Alpha -> State
    }

data NFA = NFA
    { ini :: State
    , fin :: State -> Bool
    , tra :: State -> Alpha -> [State]
    }


--- Example DFA ---

t :: State -> Alpha -> State
t "1" 'A' = "2"
t "2" 'A' = "3"
t "2" 'B' = "1"
t "2" 'C' = "3"
t "3" 'D' = "5"
t "4" 'A' = "2"

i :: State
i = "1"

f :: State -> Bool
f = (`elem` ["4","5"])

dfa = DFA i f t

--- Example NFA ---

t' :: State -> Alpha -> [State]
t' "1" 'A' = ["2","4"]
t' "2" 'A' = ["3"]
t' "2" 'B' = ["1"]
t' "2" 'C' = ["3"]
t' "3" 'D' = ["5"]
t' "4" 'A' = ["2"]
t' _   _   = []

nfa = NFA i f t'


--- Automata execution functions ---

process :: DFA -> [Alpha] -> State
-- ^ Returns the final state of a DFA given a word.
process (DFA i f t) = foldl t i

accept :: DFA -> [Alpha] -> Bool
-- ^ Checks if a certain word is accepted by a given DFA.
accept dfa@(DFA _ f _) = f . (process dfa) 


--- Automata log functions ---

logDFA t s c = writer (t s c, [s])
logNFA t s c = WriterT $ map (\x -> (x,[s])) (t s c)
addFinal (a,xs) = xs ++ [a]

executeDFA :: DFA -> [Alpha] -> [State]
-- ^ Returns the execution log of DFA given a word.
executeDFA (DFA i _ t) = addFinal . runWriter . foldM (logDFA t) i

executeNFA :: NFA -> [Alpha] -> [[State]]
-- ^ Returns the executions logs of all paths in a NFA given a word.
executeNFA (NFA i _ t) = map addFinal . runWriterT . foldM (logNFA t) i



--- Main ---

main = mapM_ putStrLn [ show $ executeDFA dfa "AAD", show $ executeNFA nfa "AA" ]
