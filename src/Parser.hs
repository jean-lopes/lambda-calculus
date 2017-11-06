module Parser
    ( parseCommand
    ) where
import           Control.Monad        (void)
import           Data.Char
import qualified Data.Set             as Set
import           Interpreter
import           LambdaCalculus
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
lambda = void $ (symbol '\\' <|> symbol 'λ')

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

identifier :: Parser String
identifier = label "identifier" $ lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

colon :: Parser ()
colon = void $ symbol ':'

commandHelper :: Char -> String -> a -> Parser a
commandHelper c' n constructor = lbl $ constructor <$ (colon >> lbl cmd)
  where
    c = toLower c'
    name = map toLower n
    cmd = string name <|> (:) <$> char c <*> pure []
    lbl = label (':':name)

quit :: Parser Command
quit = commandHelper 'q' "quit" Quit

bindings :: Parser Command
bindings = commandHelper 'b' "bindings" Bindings

reset :: Parser Command
reset = commandHelper 'r' "reset" Reset

help :: Parser Command
help = commandHelper 'h' "help" Help

eval :: Parser Command
eval = label "lambda expression" $ Eval <$> expression

synonym :: Parser Command
synonym = Synonym . map toUpper <$> identifier <* symbol '=' <*> abstraction

command :: Parser Command
command =   try synonym
        <|> try eval
        <|> try bindings
        <|> try reset
        <|> try help
        <|> quit

goal :: Parser a -> Parser a
goal p = whitespace *> lexeme p <* eof

interpreterCommand :: Parser Command
interpreterCommand = goal command

parseCommand :: String -> Either String Command
parseCommand str = case parse interpreterCommand "(input)" str of
    (Left e)    -> Left . unlines . drop 1 . lines . parseErrorPretty $ e
    (Right cmd) -> Right cmd
