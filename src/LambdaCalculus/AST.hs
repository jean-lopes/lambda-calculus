module LambdaCalculus.AST
    ( Expression(..)
    , pretty
    )where
import           Data.Monoid ((<>))

data Expression
    = Variable Char
    | Application Expression Expression
    | Abstraction Char Expression
    | Nested Expression
    deriving Show

pretty :: Expression -> String
pretty (Variable c)      = [c]
pretty (Application f g) = pretty f <> " " <> pretty g
pretty (Abstraction c e) = "λ" <> [c] <> "." <> pretty e
pretty (Nested e)        = "(" <> pretty e <> ")"
