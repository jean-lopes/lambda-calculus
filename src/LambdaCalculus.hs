module LambdaCalculus
    ( Expression(..)
    , pretty
    , evalExpr
    ) where
import           Data.Monoid ((<>))

data Expression
    = Variable Char
    | Application Expression Expression
    | Abstraction Char Expression
    | Nested Expression
    deriving (Show, Eq)

pretty :: Expression -> String
pretty (Variable c)      = [c]
pretty (Application f g) = pretty f <> " " <> pretty g
pretty (Abstraction c e) = "λ" <> [c] <> "." <> pretty e
pretty (Nested e)        = "(" <> pretty e <> ")"

evalExpr :: Expression -> Expression
evalExpr e = if e == x then x else evalExpr x
  where
    x = betaReduction e

betaReduction :: Expression -> Expression
betaReduction (Application (Nested (Abstraction c a)) b) = substitute c a b
betaReduction (Application (Abstraction c a) b) = substitute c a b
betaReduction (Application a b) = Application (betaReduction a) (betaReduction b)
betaReduction (Abstraction c e) = Abstraction c $ betaReduction e
betaReduction (Nested e)        = Nested $ betaReduction e
betaReduction (Variable c)      = Variable c

substitute :: Char -> Expression -> Expression -> Expression
substitute c new (Variable a) = if a == c then new else Variable a
substitute c new (Application f g) = Application (substitute c new f) (substitute c new g)
substitute c new (Abstraction a e) = Abstraction a $ substitute c new e
substitute c new (Nested e) = Nested $ substitute c new e

xxx :: Expression
xxx = Application (Abstraction 'x' (Variable 'x')) (Variable 'y')
