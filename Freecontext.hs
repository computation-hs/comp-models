-- Input aphabet and variable types
type Alpha = Char
type Vars  = Char


-- | A free context grammar expression is a list of
-- variables and input alphabet symbols.
-- Epsilon is the empty list.
type FExpr = [Either Alpha Vars]

-- | A free context grammar is a set of productions
-- from variables to expressions.
type FGrammar = [(Vars,Fexpr)]
