import Control.Monad.Writer

--- Basic types for automata ---

type State = String

data DFA = DFA
    { initial :: State
    , isFinal :: State -> Bool
    , transition :: State -> Char -> State
    }

data NFA = NFA
    { ini :: State
    , fin :: State -> Bool
    , tra :: State -> Char -> [State]
    }


--- Example DFA ---

t :: State -> Char -> State
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

t' :: State -> Char -> [State]
t' "1" 'A' = ["2","4"]
t' "2" 'A' = ["3"]
t' "2" 'B' = ["1"]
t' "2" 'C' = ["3"]
t' "3" 'D' = ["5"]
t' "4" 'A' = ["2"]
t' _   _   = []

nfa = NFA i f t'


--- Automata execution functions ---

process :: DFA -> [Char] -> State
process dfa = foldl (transition dfa) (initial dfa)

accept :: DFA -> [Char] -> Bool
accept dfa = (isFinal dfa) . (execTrans dfa) 

 
--- Automata log functions ---

logDFA t s c = writer (t s c, [s])
executeDFA (DFA i _ t) = execWriter . foldM (logDFA t) i


logNFA t s c = WriterT $ map (\x -> (x,[s])) (t s c)
executeNFA (NFA i _ t) = execWriterT . foldM (logNFA t) i



--- Main ---

main = mapM_ putStrLn [ show $ executeDFA dfa "AAD", show $ executeNFA nfa "AA" ]
