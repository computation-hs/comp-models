import Automata
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Applicative ((<$>))

dfaparser :: Parser DFA
nfaparser :: Parser NFA
