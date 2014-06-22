{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scheme.Error where

import Text.Parsec.Error

data SchemeError = RuntimeError String
                 | SyntaxError ParseError
  deriving Show

instance Eq SchemeError where
  _ == _ = False

instance Eq ParseError where
  _ == _ = False
