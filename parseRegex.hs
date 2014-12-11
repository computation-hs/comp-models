import Regex
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Applicative ((<$>))

{- 
  Syntax for a Regex parser
-}

regexp :: Parser Regex
regexp = buildExpressionParser optable term

optable = [ [Infix concatp AssocLeft ]
          , [Infix alternp AssocLeft ]
          ]

term :: Parser Regex
term = literalp <|> parens regexp

literalp :: Parser Regex
literalp = Literal <$> lower

alternp :: Parser (Regex -> Regex -> Regex)
alternp = char '+' >> return (Altern)

concatp :: Parser (Regex -> Regex -> Regex)
concatp = char '.' >> return (Concat)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')
