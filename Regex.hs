module Regex where

-- Input Alphabet type
type Alpha = Char


-- Regular Expression type
data Regex = Literal Alpha
           | Epsilon
           | Altern Regex Regex
           | Concat Regex Regex
           | Kleene Regex

instance Show Regex where
  show (Literal x ) = show x
  show (Epsilon   ) = "%"
  show (Altern x y) = "(" ++ show x ++ "|" ++ show y ++ ")"
  show (Concat x y) = show x ++ show y
  show (Kleene x  ) = "(" ++ show x ++ ")*"
  

-- Matching a regular expression
match :: Regex -> [Alpha] -> Bool
match (Literal x)  s = (s == [x])
match (Epsilon)    s = (s == [])
match (Altern x y) s = (match x s)&&(match y s)
match (Concat x y) s = or [(match x sa)&&(match y sb) | (sa,sb) <- partitions]
  where partitions = [splitAt n s | n <- [0..length s]]
match (Kleene x)   s = (match Epsilon s) || or [(match x sa)&&(match (Kleene x) sb) | (sa,sb) <- partitions]
  where partitions = [splitAt n s | n <- [0..length s]]
