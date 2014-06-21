{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative((<*))
import Data.Char
import Data.Symbol
import System.Environment(getArgs)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token



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
  deriving Show

-- (The last line in the definition of Expr there tells the compiler to 
-- automatically derive an instance of the Show typeclass for our new Expr 
-- datatype. This allows us to call the show function on any Expr--Haskell's 
-- equivalent to many other language's toString.)



-- Next, we'll define the rules for making identifiers in our language.
lang = emptyDef{ identStart = letter
               , identLetter = noneOf " \t\r\n\f\v()"
               , reservedNames = ["lambda"]
               , caseSensitive = True
               }

-- And then we'll use that definition to generate some tokenizers.
TokenParser{ parens     = m_paren
           , whiteSpace = m_space
           , identifier = m_ident
           , reserved   = m_reserved
           , integer    = m_int
           } = makeTokenParser lang

-- makeTokenParser returns a larger record of type TokenParser. Here, we've 
-- pattern-matched that return value against only the fields that we care about.  
-- This gives us tokenizers for sexprs (m_parens), identifiers, (m_ident), 
-- whitespace (m_space), and integers (m_int).



-- Now to define the expression parser.
scheme :: Parser Expr
scheme = buildExpressionParser [] term <?> "expression"
  where term =  do int <- m_int
                   return $ Val int
            <|> do var <- m_ident
                   return . Var $ intern var
            <|> m_paren ((do rator <- scheme
                             rand <- m_space >> scheme
                             return $ App rator rand)
                     <|> (do m_reserved "lambda"
                             ident <- m_paren m_ident
                             let var = intern ident
                             bod <- scheme
                             return $ Lam var bod))



-- The driver function for the executable parser that simply runs the parser on 
-- the first command-line argument and prints the resulting error or Expr.
main :: IO ()
main = do
  args <- getArgs
  case parse scheme "scheme" (head args) of
    Left err -> print err
    Right res -> print res
