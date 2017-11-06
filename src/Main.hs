{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main
where
import           Control.Monad.State.Lazy
import qualified Data.Map.Strict          as Map
import           Data.Monoid              ((<>))
import qualified Data.Text                as Text
import           Interpreter
import           LambdaCalculus
import           Parser
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    evalStateT exec Map.empty

exec :: (MonadState Env m, MonadIO m) => m ()
exec = do
  ls <- liftIO $ putStr "> " >> getLine
  str <- preprocessor ls
  case parseCommand str of
    (Left e)              -> (liftIO $ putStr e) >> exec
    (Right Quit)          -> return ()
    (Right Bindings)      -> bindings >> exec
    (Right Reset)         -> reset >> exec
    (Right Help)          -> help >> exec
    (Right (Eval e))      -> eval e >> exec
    (Right (Synonym n e)) -> synonym (n, e) >> exec

bindings :: (MonadState Env m, MonadIO m) => m ()
bindings = do
    env <- get
    let ls = Map.toAscList $ env
        ms = if null ls
            then "No bindings!\n"
            else unlines $ "Bindings:" : map synToStr ls
    liftIO $ putStr ms
  where
    synToStr = \(n, e) -> n <> " = " <> pretty e

reset :: MonadState Env m => m ()
reset = put $ Map.empty

help :: MonadIO m => m ()
help = liftIO $ putStrLn "this is help."

eval :: MonadIO m => Expression -> m ()
eval e = do
  liftIO . putStrLn . show $ e
  liftIO . putStrLn . pretty . evalExpr $ e

synonym :: (MonadState Env m) => (String, Expression) -> m ()
synonym (name, expr) = modify (Map.insert name expr)

preprocessor :: (MonadState Env m) => String -> m String
preprocessor str = do
    env <- get
    let txt = foldr replateSyn (Text.pack str) $ Map.toList env
    return $ Text.unpack txt
  where
    replateSyn (name, expr) txt =
      let syn = Text.pack name
          exprStr = Text.pack $ pretty $ Nested expr
      in  Text.replace syn exprStr txt
