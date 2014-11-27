import Control.Monad.Writer

--- Tipos ---

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


--- DFA de ejemplo ---

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

--- NFA de ejemplo ---

t' :: State -> Char -> [State]
t' "1" 'A' = ["2"]
t' "2" 'A' = ["3"]
t' "2" 'B' = ["1"]
t' "2" 'C' = ["3"]
t' "3" 'D' = ["5"]
t' "4" 'A' = ["2"]

nfa = NFA i f t'

--- Funciones con log ---

logt t s c = writer ((t s c), [s])
execution (DFA i f t) = execWriter . foldM (logt t) i

main = putStrLn $ show $ execution dfa ['A','A','D']
