module Regex where

-- Tipo para el alfabeto de entrada
type Alpha = Char

-- Expresión regular
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
  

-- Cadenas que cumplen una expresión regular
match :: Regex -> [Alpha] -> Bool
match (Literal x)  s = (s == [x])
match (Epsilon)    s = (s == [])
match (Altern x y) s = (match x s) && (match y s)
match (Concat x y) s = or [(match x sa)&&(match y sb) | (sa,sb) <- partitions]
match (Kleene x)   s = (match Epsilon s) || or [(match x sa)&&(match (Kleene x) sb) | (sa,sb) <- partitions]
  where partitions = [splitAt n s | n <- [0..length s]]
