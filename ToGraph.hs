module ToGraph where

import Automata
import ToString
import Convergence
import Data.Maybe
import Data.List

header = "digraph {\n"
end    = "}"

-- Lists states
listStates :: DFA -> [State]
listStates dfa = stabilize (nub . concat . (map (newStates dfa))) [fromJust (initialDFA dfa)]

newStates :: DFA -> State -> [State]
newStates dfa s = s : catMaybes [(deltaDFA dfa) s a | a <- (alphaDFA dfa)]


-- Writes graph
productions :: DFA -> [String]
productions dfa = catMaybes [fmap (format s a) (delta s a) | a <- alpha, s <- states]
      where alpha  = alphaDFA dfa
            states = listStates dfa
            delta  = deltaDFA dfa
            format s a t = "\t" ++ toString s ++ " -> " ++ toString t 
                                ++ " label[\"" ++ toString a ++ "\"]"

toDot :: DFA -> String
toDot dfa = header ++
            unlines (productions dfa) ++
            end
