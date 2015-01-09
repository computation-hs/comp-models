module ToGraph(toDot) where

import Automata
import ToString
import Convergence
import Data.Maybe
import Data.List



-- Auxiliary function. Lists states
listStates :: DFA -> [State]
listStates dfa = stabilize (nub . concat . (map (newStates dfa))) [fromJust (initialDFA dfa)]

newStates :: DFA -> State -> [State]
newStates dfa s = s : catMaybes [(deltaDFA dfa) s a | a <- (alphaDFA dfa)]

listStatesNfa :: NFA -> [State]
listStatesNfa nfa = stabilize (nub . concat . (map (newStatesNfa nfa))) (initialNFA nfa)

newStatesNfa :: NFA -> State -> [State]
newStatesNfa nfa s = s : concat [(deltaNFA nfa) s a | a <- (alphaNFA nfa)]

-- Writes graph
-- | 'toDot' returns a formatted string, containing the representation of the
--   automata as a .dot graph.
toDot :: NFA -> String
toDot nfa = header ++
            unlines (edges nfa) ++
            end
    where
      header = "digraph {\n"
      end    = "}"


-- | 'edgesNfa' creates a list of edges of formatted edges of the representation
--    graph of the automata. An edge will represent a transition and will be 
--    labeled with the alphabet input.
edgesNfa :: NFA -> [String]
edgesNfa nfa = concat [fmap (format s a) (delta s a) | a <- alpha, s <- states]
    where alpha  = alphaNFA nfa
          states = listStatesNfa nfa
          delta  = deltaNFA nfa
          format s a t = "\t" ++ toString s ++ " -> " ++ toString t 
                              ++ " label[\"" ++ toString a ++ "\"]"
