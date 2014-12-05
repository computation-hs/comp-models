-- Propuesta de tipos para la gramática libre de contexto

-- Una expresión en una gramática libre de contexto
-- es una lista de variables y símbolos del alfabeto. 
type FExpr v a = [Either a v]

-- Una gramática libre de contexto la forman un conjunto
-- de producciones de variables a expresiones.
type FGrammar v a = [(v,Fexpr a v)]
