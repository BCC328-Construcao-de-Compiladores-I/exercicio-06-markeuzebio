module L.L2.Interpreter.Interp where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))

import L.L2.Frontend.Syntax

import Utils.Pretty
import Utils.Value
import Utils.Var

type Env = Map Var Value

readValue :: IO Value
readValue = handleInput <$> getLine
  where
    handleInput s = fromMaybe (VStr s) (VInt <$> readMaybe s)

evalL2 :: L2 -> IO (Either String Env)
evalL2 (L2 ss)
  = foldM step (Right Map.empty) ss
  where
    step ac@(Left _) _  = pure ac
    step (Right env) s2 = evalS2 env s2

-- Statements evaluation
evalS2 :: Env -> S2 -> IO (Either String Env)
evalS2 env (LAssign id e)
  = case evalE2 env e of
      Right val -> pure (Right $ Map.insert id val env)
      Left err  -> pure $ Left err
evalS2 env (LRead str id)
  = do
    putStr str
    val <- readValue
    pure (Right $ Map.insert id val env)
evalS2 env (LPrint e)
  = case evalE2 env e of
    Left err -> pure $ Left err
    Right val -> do
      putStrLn (pretty val)
      pure (Right env)
evalS2 env (LDef id e ss)
  = case evalE2 env e of
      Left  err -> pure $ Left err
      Right val -> do
        let env2 = Map.insert id val env
        innerBlockEvaluation <- foldM step (Right env2) ss
        case innerBlockEvaluation of
          Left err -> pure $ Left err
          Right _  -> pure $ Right env
  where
    step ac@(Left _) _  = pure ac
    step (Right env) s2 = evalS2 env s2

-- Expressions evaluation
evalE2 :: Env -> E2 -> Either String Value
evalE2 _ (LVal v)    = Right v
evalE2 env (LVar id)
  = case Map.lookup id env of
      Just val -> Right val
      Nothing  -> Left ("Undefined variable " ++ pretty id)
evalE2 env (LAdd e1 e2)
  = do
    v1 <- evalE2 env e1
    v2 <- evalE2 env e2
    v1 .+. v2
evalE2 env (LMinus e1 e2)
  = do
    v1 <- evalE2 env e1
    v2 <- evalE2 env e2
    v1 .-. v2
evalE2 env (LMul e1 e2)
  = do
    v1 <- evalE2 env e1
    v2 <- evalE2 env e2
    v1 .*. v2
evalE2 env (LDiv e1 e2)
  = do
    v1 <- evalE2 env e1
    v2 <- evalE2 env e2
    v1 ./. v2
