module Scheme ( eval ) where

import Prelude hiding(lookup)

import Scheme.Error
import Scheme.Parser
import Scheme.Types



-- | The main interpreter. @eval@ parses the input string with @parseScheme@
-- and sends the result to @evalExpr@ with an empty environment.
eval :: String -> Either SchemeError Value
eval input = case parseScheme input of
  Left err   -> Left $ SyntaxError err
  Right expr -> evalExpr expr emptyEnv

-- | A basic lexical interpreter for our extended lambda calculus defined by 
-- @Expr@ in @Scheme.Types@.
evalExpr :: Expr -> Env -> Either SchemeError Value
evalExpr expr env = case expr of
  Val n     -> Right $ IntVal n
  Var v     -> maybeUnbound v $ lookup env v
  Lam v bod -> Right $ LamVal v bod env
  App rator rand -> do
    x <- evalExpr rand env
    f <- evalExpr rator env
    attemptApply f x

  where maybeUnbound :: Name -> Maybe Value -> Either SchemeError Value
        maybeUnbound v Nothing  = Left . RuntimeError $ "attempt to reference unbound variable " ++ show v
        maybeUnbound _ (Just x) = Right x

        attemptApply :: Value -> Value -> Either SchemeError Value
        attemptApply (LamVal v bod env') x = evalExpr bod $ extend env' v x
        attemptApply value _ = Left . RuntimeError $ "attempt to apply non-procedure " ++ show value
