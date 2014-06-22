module Main where

import Test.Hspec

import Data.Symbol
import qualified Text.Parsec.Error

import Scheme.Parser



instance Eq Text.Parsec.Error.ParseError where
  _ == _ = False



main :: IO ()
main = hspec $ do

  describe "parseScheme" $ do
    it "parses integers" $ do
      parseScheme "42" `shouldBe` Right (Val 42)
    it "parses names" $ do
      parseScheme "x" `shouldBe` Right (Var $ intern "x")
    it "parses lambda sexprs" $ do
      parseScheme "(lambda (x) x)" `shouldBe` Right (Lam (intern "x") (Var $ intern "x"))
    it "parses application sexprs" $ do
      parseScheme "(x x)" `shouldBe` Right (App (Var $ intern "x") (Var $ intern "x"))
