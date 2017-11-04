module LambdaCalculus.Parser
    ( parseLambdaExpr
    ) where
import           Control.Monad        (void)
import           Data.Char
import qualified Data.Set             as Set
import           LambdaCalculus.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char

data LambdaError
    = InvalidVariable Char
    deriving (Show, Ord, Eq)

type Parser = Parsec LambdaError String

instance ShowErrorComponent LambdaError where
    showErrorComponent (InvalidVariable c) = "Invalid variable '" ++ [c] ++ "'"

whitespace :: Parser ()
whitespace = void $ many spaceChar

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: Char -> Parser Char
symbol c = char c <* whitespace

parens :: Parser a -> Parser a
parens p = symbol '(' *> p <* symbol ')'

dot :: Parser Char
dot = symbol '.'

var :: Parser Char
var = do
    c <- letterChar <?> "variable"
    if isLower c
        then return c
        else fancyFailure . Set.singleton . ErrorCustom . InvalidVariable $ c

lambda :: Parser ()
lambda = void $ label "lambda" (symbol '\\' <|> symbol 'λ')

variable :: Parser Expression
variable = Variable <$> var

application :: Parser (Expression -> Expression -> Expression)
application = lexeme $ pure Application

abstraction :: Parser Expression
abstraction = Abstraction <$> (lambda *> lexeme var <* dot) <*> expression

nested :: Parser Expression
nested = Nested <$> parens expression

operand :: Parser Expression
operand = abstraction <|> nested <|> variable

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (op <*> pure x <*> p >>= rest) <|> return x

expression :: Parser Expression
expression = chainl1 operand application

parseLambdaExpr :: String -> Either String Expression
parseLambdaExpr str = case parse fullExpr "<input>" str of
    (Left e)     -> Left $ parseErrorPretty e
    (Right expr) -> Right expr
  where
    fullExpr = whitespace *> lexeme expression <* eof
