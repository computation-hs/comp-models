{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module ToGraph(toDot) where


import Automata
import ToString
import Convergence
import Data.Maybe
import Data.List

-- | 'IsGraph' is the class of the types that can be represented in .dot format.
class IsGraph a where
    -- | 'toDot' transforms to .dot format.
    toDot :: a -> String

instance (Automata a m) => IsGraph a where
    toDot = nfaToDot . toNFA


-- Writes graph
-- | 'toDot' returns a formatted string, containing the representation of the
--   automata as a .dot graph.
nfaToDot :: NFA -> String
nfaToDot nfa = header ++
               unlines (edges nfa) ++
               end
    where
      header = "digraph {\n"
      end    = "}"


-- | 'edges' creates a list of edges of formatted edges of the representation
--    graph of the automata. An edge will represent a transition and will be 
--    labeled with the alphabet input.
edges :: NFA -> [String]
edges nfa = concat [fmap (format s a) (delt s a) | a <- alph, s <- stat]
    where alph = alpha nfa
          stat = states nfa
          delt = delta nfa
          format s a t = "\t" ++ toString s ++ " -> " ++ toString t 
                              ++ " label[\"" ++ toString a ++ "\"]"
