module Main where

import Prelude hiding(print, putStrLn)
import System.Environment(getArgs)
import System.IO.Streams

import Repl
import Scheme(eval)
import Scheme.Types.Mocks(MockIO, print, putStrLn)



main :: IO ()
main = do
  args <- getArgs
  case args of
    [expr] -> printEval expr
    []     -> repl ',' printEval
    _      -> putStrLn "Invalid arguments"

printEval :: MockIO io => String -> io ()
printEval = either print print . eval
