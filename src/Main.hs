module Main where

import System.Environment(getArgs)

import Scheme(eval)



-- The driver function for the executable parser that simply runs the parser on 
-- the first command-line argument and prints the resulting error or Expr.
main :: IO ()
main = do
  args <- getArgs
  case eval (head args) of
    Left err -> print err
    Right res -> print res
