module ToGraph(toDot) where

import Automata
import ToString
import Convergence
import Data.Maybe
import Data.List



-- Lists states
listStates :: DFA -> [State]
listStates dfa = stabilize (nub . concat . (map (newStates dfa))) [fromJust (initialDFA dfa)]

newStates :: DFA -> State -> [State]
newStates dfa s = s : catMaybes [(deltaDFA dfa) s a | a <- (alphaDFA dfa)]


-- Writes graph
-- | 'toDot' returns a formatted string, containing the representation of the
--   automata as a .dot graph.
toDot :: DFA -> String
toDot dfa = header ++
            unlines (transitionsDot dfa) ++
            end
    where
      header = "digraph {\n"
      end    = "}"


-- | 'transitions' creates a list of formatted edges of the representation graph
--   of the automata, an edge will represent a transition and will be labeled
--   with the alphabet input that generates the transition.
transitionsDot :: DFA -> [String]
transitionsDot dfa = catMaybes [fmap (format s a) (delta s a) | a <- alpha, s <- states]
      where alpha  = alphaDFA dfa
            states = listStates dfa
            delta  = deltaDFA dfa
            format s a t = "\t" ++ toString s ++ " -> " ++ toString t
                                ++ " label[\"" ++ toString a ++ "\"]"
