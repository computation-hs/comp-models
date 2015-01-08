module ToGraph where

import Automata
import ToString
import Data.Maybe
import Data.List

header = "digraph {"
end    = "}"

-- Convergence
converge :: (Eq a) => [a] -> a
converge (x:ys@(y:_))
    | x == y    = y
    | otherwise = converge ys 

stabilize :: (Eq a) => (a -> a) -> (a -> a)
stabilize f x = converge $ iterate f x

-- Lists states
listStates :: DFA -> [State]
listStates dfa = stabilize (nub . concat . (map (newStates dfa))) [fromJust (initialDFA dfa)]

newStates :: DFA -> State -> [State]
newStates dfa s = s : catMaybes [(deltaDFA dfa) s a | a <- (alphaDFA dfa)]


-- Writes graph
toDot :: DFA -> [String]
toDot dfa = catMaybes [fmap (format s a) (delta s a) | a <- alpha, s <- states]
      where alpha  = alphaDFA dfa
            states = listStates dfa
            delta  = deltaDFA dfa
            format s a = ((toString s ++ "," ++ toString a ++ ",") ++) . toString

{-
toGraph :: DFA -> String
toGraph dfa = stateToGraph s



stateToGraph :: DFA -> State -> String
stateToGraph dfa s = concat $ map alphaToGraph (newStates dfa s)

alphaToGraph :: DFA -> State -> Alpha -> String
alphaToGraph dfa s a = fromMaybe "" (fmap (transitionToGraph s a) $ (delta dfa) s a)

transitionToGraph :: State -> Alpha -> State -> String
transitionToGraph s a t = 
    (toString s) ++ " -> " ++ (toString t) ++
    "[label=" ++ (toString a) ++ "]"



-- Convergence
converge :: [a] -> a
converge (x:ys@(y:_))
    | x == y    = y
    | otherwise = converge ys 

stabilize :: (a -> a) -> (a -> a)
stabilize = converge . iterate 
-}
