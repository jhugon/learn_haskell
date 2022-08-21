module ChebyshevSpec (spec) where
import SpecHelper
import Chebyshev

spec :: Spec
spec = do
  describe "factorial function" $ do
    it "can compute the n = 0 case" $ do
      (factorial 0) `shouldBe` Just 1
    it "can compute the n = 1 case" $ do
      (factorial 1) `shouldBe` Just 1
    it "can compute the n = 10 case" $ do
      (factorial 10) `shouldBe` Just 3628800
    it "can compute the n = 10 case" $ do
      (factorial 15) `shouldBe` Just 1307674368000 
    it "can't compute the n = -1 case" $ do
      (factorial (-1)) `shouldBe` Nothing
  describe "fionacci function" $ do
    it "can compute the n = 0 case" $ do
      (fibonacci 0) `shouldBe` 0
    it "can compute the n = 1 case" $ do
      (fibonacci 1) `shouldBe` 1
    it "can compute the n = 2 case" $ do
      (fibonacci 2) `shouldBe` 1
    it "can compute the n = 3 case" $ do
      (fibonacci 3) `shouldBe` 2
    it "can compute the n = 10 case" $ do
      (fibonacci 10) `shouldBe` 55
    it "can compute the n = 10 case" $ do
      (fibonacci 15) `shouldBe` 610
    it "can't compute the n = -1 case" $ do
      (fibonacci (-1)) `shouldBe` 610
