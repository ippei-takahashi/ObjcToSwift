import Control.Applicative ((<$>),(*>),(<*),pure)
import Data.Char (ord)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser, makeTokenParser, reservedOpNames)
import qualified Text.Parsec.Token as P

lexer = P.makeTokenParser $ P.LanguageDef 
	"/*"
	"*/"
	"//"
	True
	(letter <|> char '_')
	(alphaNum <|> char '_')
	(oneOf ":!#$%&*+./<=>?\\^|-~")
	(oneOf ":!#$%&*+./<=>?\\^|-~")
	[]
	[]
	True
	

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
operator = P.operator lexer
reservedOp = P.reservedOp lexer
symbol = P.symbol lexer

data Class = Class Extends Exprs
data Extends = Extends
data Exprs = Exprs
data Exp = Add Exp Exp | Mul Exp Exp | Nat Int deriving Show

clazz :: Parser String
clazz = do
	many $ identifier <|> operator
	whiteSpace
	string "@interface"
	whiteSpace
	ds <- many1 digit
	return ds

-- expr ::= term ('+' expr | ε)
expr :: Parser Exp
expr = 
	do 
		t <- term
		(Add t <$> (char '+' *> expr)) <|> pure t

-- term ::= factor ('*' term | ε)
term :: Parser Exp
term = 
	do
		f <- factor
		(Mul f <$> (char '*' *> term)) <|> pure f    

-- factor ::= '(' expr ')' | nat
factor :: Parser Exp    
factor = (char '(' *> expr <* char ')') <|> nat

-- nat ::= '0' | '1' | '2' | ...
nat :: Parser Exp
nat = Nat . charToInt <$> oneOf ['0'..'9'] where
	charToInt c = ord c - ord '0'