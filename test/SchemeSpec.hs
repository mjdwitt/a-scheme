module SchemeSpec where

import Test.Hspec

import Scheme
import Scheme.Types

import SpecUtils



spec :: Spec
spec = do

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
