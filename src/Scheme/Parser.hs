{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Scheme.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

-- First, we'll import our internal types for expressions
import Scheme.Types (Expr(..), intern)



-- Next, we'll define the rules for making identifiers in our language.
lang = emptyDef{ identStart = noneOf " \t\r\n\f\v()"
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

parseScheme :: String -> Either ParseError Expr
parseScheme = parse scheme "scheme"
