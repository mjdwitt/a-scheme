module SchemeSpec.ParserSpec where

import Test.Hspec

import Data.Symbol

import Scheme.Parser
import Scheme.Types

import SpecUtils



spec :: Spec
spec = do

  describe "parseScheme" $ do
    it "parses integers" $ do
      parseScheme "42" `shouldBe` Right (Val 42)
    it "parses names" $ do
      parseScheme "x" `shouldBe` Right (Var $ intern "x")
    it "parses lambda sexprs" $ do
      parseScheme "(lambda (x) x)" `shouldBe` Right (Lam (intern "x") (Var $ intern "x"))
    it "parses application sexprs" $ do
      parseScheme "(x x)" `shouldBe` Right (App (Var $ intern "x") (Var $ intern "x"))
    it "parses complicated sexprs" $ do
      (parseScheme "(((lambda (x) (lambda (v) x)) 42) 7)" `shouldBe`) $ Right
        (App (App (Lam (intern "x") (Lam (intern "v") (Var $ intern "x")))
                  (Val 42))
             (Val 7))
      (parseScheme "(((lambda (x) (lambda (f) (f x))) 42) (lambda (y) y))" `shouldBe`) $ Right
        (App (App (Lam (intern "x")
                    (Lam (intern "f")
                      (App (Var $ intern "f") (Var $ intern "x"))))
                  (Val 42))
             (Lam (intern "y") (Var $ intern "y")))
    it "errors on invalid input" $ do
      parseScheme "" `shouldSatisfy` isError
      parseScheme "(x)" `shouldSatisfy` isError
      parseScheme "(x x x)" `shouldSatisfy` isError
