module Main where

import Test.Hspec

import Data.Symbol

import Scheme
import Scheme.Error()
import Scheme.Parser
import Scheme.Types



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

  describe "eval" $ do
    it "evaluates integers" $ do
      eval "42" `shouldBe` Right (IntVal 42)
    it "errors on unbound variables" $ do
      eval "x" `shouldSatisfy` isError
    it "doesn't evaluate lambda bodies until application" $ do
      eval "(lambda (x) y)" `shouldSatisfy` not . isError
    it "evaluates lambda bodies on application" $ do
      eval "((lambda (x) y) 42)" `shouldSatisfy` isError
    context "returns evaluated lambda bodies" $ do
      it "for constant bodies" $ do
        eval "((lambda (x) 42) 7)" `shouldBe` Right (IntVal 42)
      it "for variable bodies" $ do
        eval "((lambda (x) x) 42)" `shouldBe` Right (IntVal 42)
      it "for complex lambdas" $ do
        eval "(((lambda (x) (lambda (v) x)) 42) 7)" `shouldBe` Right (IntVal 42)
      it "for complex arguments" $ do
        eval "(((lambda (x) (lambda (f) (f x))) 42) (lambda (y) y))" `shouldBe` Right (IntVal 42)

    where isError :: Either error value -> Bool
          isError (Right _) = False
          isError (Left _)  = True
