module Convergence where

-- Convergence
converge :: (Eq a) => [a] -> a
converge (x:ys@(y:_))
    | x == y    = y
    | otherwise = converge ys 

stabilize :: (Eq a) => (a -> a) -> (a -> a)
stabilize f x = converge $ iterate f x
