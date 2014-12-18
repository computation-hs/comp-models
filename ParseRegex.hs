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

optable = [ [ Postfix starp ]
          , [ Infix concatp AssocLeft ]
          , [ Infix alternp AssocLeft ]
          ]

term :: Parser Regex
term =  literalp
       <|> epsilonp
       <|> parens regexp

literalp :: Parser Regex
literalp = Literal <$> lower

epsilonp :: Parser Regex
epsilonp = char '%' >> return (Epsilon)

alternp :: Parser (Regex -> Regex -> Regex)
alternp = char '+' >> return (Altern)

concatp :: Parser (Regex -> Regex -> Regex)
concatp = char '.' >> return (Concat)

starp :: Parser (Regex -> Regex)
starp = char '*' >> return (Kleene)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')


{-
 Test Main
-}
main = do
  input <- getLine
  let iregx = parse regexp "" input
  let r = case iregx of
        Right x -> x
        Left  r -> Epsilon
  putStrLn ("Regular Expression: " ++ show r)
  putStrLn ("Insert expressions:")
  interact (unlines . map (\xs -> 
                            if match r xs
                            then "match"
                            else "does not match")
            . lines)
