module Regex where

-- | 'Alpha' is a type synonym of Char for regular expression symbols.
type Alpha = Char


-- | 'Regex' is the type for regular expressions.
--    A Regex can be constructed as a literal alphabet symbol, the epsilon 
--    symbol, the sum or the concatenation of two expressions, or the
--    Kleene star of a regular expression.
data Regex = Literal Alpha
           | Epsilon
           | Altern Regex Regex
           | Concat Regex Regex
           | Kleene Regex

instance Show Regex where
  show (Literal x ) = show x
  show (Epsilon   ) = "ε"
  show (Altern x y) = "(" ++ show x ++ "|" ++ show y ++ ")"
  show (Concat x y) = show x ++ show y
  show (Kleene x  ) = "(" ++ show x ++ ")*"
  

-- | 'match' returns True if the alphabet string input is in the language
--    generated by the regular expression.
match :: Regex -> [Alpha] -> Bool
match (Literal x)  s = (s == [x])
match (Epsilon)    s = (s == [])
match (Altern x y) s = (match x s)||(match y s)
match (Concat x y) s = or [(match x sa)&&(match y sb) | (sa,sb) <- partitions]
  where partitions = [splitAt n s | n <- [0..length s]]
match (Kleene x)   s = (match Epsilon s) || or [(match x sa)&&(match (Kleene x) sb) | (sa,sb) <- partitions]
  where partitions = [splitAt n s | n <- [0..length s]]
