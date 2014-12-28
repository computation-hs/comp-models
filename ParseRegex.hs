import Regex
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Applicative ((<$>))

{- Syntax for a RegExp parser -}

-- | 'RegexC' is a common Char Regex.
type RegexC = Regex Char

-- | 'regexp' is the complete RegExp parser. It is built as a expression parser
--   using 'buildExpressionParser' with the following operations:
--
--    * Kleene Star ('starp' parser) 
--    * Product ('concatp' parser)
--    * Sum ('alternp' parser)
--
--   Returns the read regular expression.
regexp :: Parser RegexC
regexp = buildExpressionParser optable term
  where
    optable = [ [ Postfix starp ]
              , [ Infix concatp AssocLeft ]
              , [ Infix alternp AssocLeft ]
              ]

-- | 'term' parses simple regexp terms. A simple term can be a literal,
--   an epsilon term or a parenthesized regular expression. 
term :: Parser RegexC
term =  literalp
       <|> epsilonp
       <|> parens regexp

-- | 'literalp' parses literals. A literal is a lowercase character.
literalp :: Parser RegexC
literalp = Literal <$> lower

-- | 'epsilonp' parses epsilons. The symbol '%' is used for epsilon.
epsilonp :: Parser RegexC
epsilonp = char '&' >> return (Epsilon)

-- | 'alternp' parses the Or operation. Consumes a '+' symbol.
alternp :: Parser (RegexC -> RegexC -> RegexC)
alternp = char '+' >> return (Altern)

-- | 'concatp' parses the Concatenation operation. It does not consume anything. 
concatp :: Parser (RegexC -> RegexC -> RegexC)
concatp = return (Concat)

-- | 'starp' parses the Kleene star operation. Consumes a '*' symbol.
starp :: Parser (RegexC -> RegexC)
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
