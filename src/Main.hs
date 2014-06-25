module Main where

import System.Environment(getArgs)
import System.IO

import Scheme(eval)



-- The driver function for the executable parser that simply runs the parser on 
-- the first command-line argument and prints the resulting error or Expr.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [expr] -> printEval expr
    []     -> repl
    _      -> putStrLn "Invalid arguments"

repl :: IO ()
repl = do
  putStr "> " >> hFlush stdout
  input <- getLine
  case input of
    ',':command -> executeReplCommand command
    expr -> printEval expr >> repl

executeReplCommand :: String -> IO ()
executeReplCommand "quit" = putStrLn "Bye bye."
executeReplCommand "help" = do
  putStrLn "Available commands:"
  putStrLn ",quit: exit this repl session"
  putStrLn ",help: print this help message"
  repl
executeReplCommand command = do
  putStr "Invalid repl command "
  print command
  executeReplCommand "help"

printEval :: String -> IO ()
printEval = either print print . eval
