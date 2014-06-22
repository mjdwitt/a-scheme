module Scheme.Types 
  ( Expr(..)
  , Name
  , intern
  , Value(..)
  ) where

import Data.Symbol



-- We'll use Symbols for names in our language. Symbols aren't built in to 
-- Haskell, but they can be easily added via the symbol package and created and 
-- inspected using the @intern@ and @unintern@ functions in @Data.Symbol@.
type Name = Symbol

-- Our language will start as a basic lambda calculus plus integers.
--
--      Expr ::= int
--             | var
--             | (lambda (name) Expr)
--             | (Expr Expr)
--
--  This BNF is isomorphic to the following Haskell algebraic datatype 
--  definition:
data Expr = Val Integer
          | Var Name
          | Lam Name Expr
          | App Expr Expr
  deriving (Show, Eq)

-- (The last line in the definition of Expr there tells the compiler to 
-- automatically derive an instance of the Show typeclass for our new Expr 
-- datatype. This allows us to call the show function on any Expr--Haskell's 
-- equivalent to many other language's toString.)



-- Our interpreter will evaluate Exprs and return Integers or Lambdas wrapped in 
-- a Value datatype.
data Value = IntVal Integer
           | LamVal Name Expr
  deriving (Eq)

-- We won't bother with displaying any extra information about an evaluated 
-- lambda expression.
instance Show Value where
  show (IntVal n) = show n
  show (LamVal _ _) = "#<procedure>"