module ToGraph where

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

{- ToDot class -}
-- | 'ToDot' class represents the types that are representable in .dot format.               
class ToDot a where
    -- | 'toDot' returns a formatted string, containing the representation of the 
    --   input as a .dot graph.
    toDot :: a -> String
    toDot x = header
           ++ unlines (edges x)
           ++ end
        where
          header = "digraph {\n"
          end    = "}"

    -- | 'edges' creates a list of formatted edges of the representation graph
    --   of the input.
    edges :: a -> [String]


instance ToDot DFA where
    edges = edgesDfa

instance ToDot NFA where
    edges = edgesNfa

-- | 'edgesDfa' creates a list of edges of the automata, an edge will represent a 
--   transition and will be labeled with the alphabet input that generates the 
--   transition.
edgesDfa :: DFA -> [String]
edgesDfa dfa = catMaybes [fmap (format s a) (delta s a) | a <- alpha, s <- states]
      where alpha  = alphaDFA dfa
            states = listStates dfa
            delta  = deltaDFA dfa
            format s a t = "\t" ++ toString s ++ " -> " ++ toString t 
                                ++ " label[\"" ++ toString a ++ "\"]"

-- | 'edgesNfa' creates a list of edges of the automata.
edgesNfa :: NFA -> [String]
edgesNfa nfa = concat [fmap (format s a) (delta s a) | a <- alpha, s <- states]
    where alpha  = alphaNFA nfa
          states = listStatesNfa nfa
          delta  = deltaNFA nfa
          format s a t = "\t" ++ toString s ++ " -> " ++ toString t 
                              ++ " label[\"" ++ toString a ++ "\"]"
