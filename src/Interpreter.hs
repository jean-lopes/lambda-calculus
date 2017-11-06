module Interpreter
    ( Command(..)
    , Env
    ) where
import           Data.Map.Strict (Map)
import           LambdaCalculus

type Env = Map String Expression

data Command
    = Quit
    | Bindings
    | Reset
    | Help
    | Eval Expression
    | Synonym String Expression
    deriving Show
