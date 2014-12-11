import Regex
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
--import Text.Parsec.Token
import Control.Applicative ((<$>))
import Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

{- 
  Syntax for a Regex parser
-}

regexp = buildExpressionParser optable term

optable = [ [Infix concatp AssocLeft ]
          , [Infix alternp AssocLeft ]
          ]

term = literalp <|> Main.parens regexp
literalp = Literal <$> lower

alternp = string "+" >> return (Altern)
concatp = string "." >> return (Concat)


-- parens (?)
lexer = P.makeTokenParser haskellDef
parens = P.parens lexer
