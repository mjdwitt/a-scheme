module SchemeSpec.TypesSpec where

import Test.Hspec

import Prelude hiding(lookup)

import Scheme.Types



spec :: Spec
spec = do

  describe "Value" $ do
    let x = IntVal 42
    let y = IntVal 7
    let z = LamVal (intern "x") (Val 7) emptyEnv
    it "can be displayed" $ do
      show x `shouldBe` "42"
      show y `shouldBe` "7"
      show z `shouldBe` "#<procedure>"
    context "Eq" $ do
      it "can compare IntVals" $ do
        x == x `shouldBe` True
        y == y `shouldBe` True
        x == y `shouldBe` False
      it "cannot compare LamVals" $ do
        z == z `shouldBe` False
      it "cannot compare different types of Values" $ do
        x == z `shouldBe` False
        y == z `shouldBe` False

  describe "Env" $ do
    let x = intern "x"
    let v = IntVal 42
    context "when empty" $ do
      it "has no entries" $ do
        lookup emptyEnv x `shouldBe` Nothing
      it "can store entires" $ do
        let env = extend emptyEnv x v
        lookup env x `shouldBe` Just v
