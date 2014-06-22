module Scheme ( eval ) where

import Data.Map.Lazy
import Prelude hiding(lookup)

import Scheme.Error
import Scheme.Parser
import Scheme.Types



eval :: String -> Either SchemeError Value
eval input = case parseScheme input of
  Left err   -> Left $ SyntaxError err
  Right expr -> evalExpr expr emptyEnv

evalExpr :: Expr -> Env -> Either SchemeError Value
evalExpr expr env = case expr of
  Val n     -> Right $ IntVal n
  Var v     -> maybeUnbound v $ lookup v env
  Lam v bod -> Right $ LamVal v bod
  App rator rand -> do
    x <- evalExpr rand env
    f <- evalExpr rator env
    case f of
      IntVal _     -> Left . RuntimeError $ "attempt to apply non-procedure " ++ show f
      LamVal v bod -> evalExpr bod $ insert v x env
  
  where maybeUnbound :: Name -> Maybe Value -> Either SchemeError Value
        maybeUnbound v Nothing  = Left . RuntimeError $ "attempt to reference unbound variable " ++ show v
        maybeUnbound _ (Just x) = Right x

type Env = Map Name Value

emptyEnv :: Env
emptyEnv = empty
