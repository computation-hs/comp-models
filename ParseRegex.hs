import Regex
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Applicative ((<$>))

{- Syntax for a RegExp parser -}

-- | 'regexp' is the complete RegExp parser. It is built as a expression parser
--   using 'buildExpressionParser' with the following operations:
--
--    * Kleene Star ('starp' parser) 
--    * Product ('concatp' parser)
--    * Sum ('alternp' parser)
--
--   Returns the read regular expression.
regexp :: Parser Regex
regexp = buildExpressionParser optable term
  where
    optable = [ [ Postfix starp ]
              , [ Infix concatp AssocLeft ]
              , [ Infix alternp AssocLeft ]
              ]

-- | 'term' parses simple regexp terms. A simple term can be a literal,
--   an epsilon term or a parenthesized regular expression. 
term :: Parser Regex
term =  literalp
       <|> epsilonp
       <|> parens regexp

-- | 'literalp' parses literals. A literal is a lowercase character.
literalp :: Parser Regex
literalp = Literal <$> lower

-- | 'epsilonp' parses epsilons. The symbol '%' is used for epsilon.
epsilonp :: Parser Regex
epsilonp = char '%' >> return (Epsilon)

-- | 'alternp' parses the Or operation. Consumes a '+' symbol.
alternp :: Parser (Regex -> Regex -> Regex)
alternp = char '+' >> return (Altern)

-- | 'concatp' parses the Concatenation operation. It does not consume anything. 
concatp :: Parser (Regex -> Regex -> Regex)
concatp = return (Concat)

-- | 'starp' parses the Kleene star operation. Consumes a '*' symbol.
starp :: Parser (Regex -> Regex)
starp = char '*' >> return (Kleene)

-- | 'parser' takes a parser and returns the same parser recognizing only
--   expressions inside two parentheses.
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')



{- Test Main -}
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
