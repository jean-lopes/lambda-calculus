module Main
where
import           LambdaCalculus
import           Repl.String

main :: IO ()
main = runRepl defaultRepl
  { replEval = exec
  , replBreak = ":q"
  }

exec :: String -> String
exec str = case parseLambdaExpr str of
  (Left e)     -> e
  (Right expr) -> pretty expr
