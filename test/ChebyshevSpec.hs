module ChebyshevSpec (spec) where
import SpecHelper
import Chebyshev

floatingEqualsMaybe :: Maybe Double -> Double -> Maybe Bool
floatingEqualsMaybe (Just x) y = Just ((x-y) < 1e-6)
floatingEqualsMaybe Nothing _ = Nothing

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
      (fibonacci 0) `shouldBe` Just 0
    it "can compute the n = 1 case" $ do
      (fibonacci 1) `shouldBe` Just 1
    it "can compute the n = 2 case" $ do
      (fibonacci 2) `shouldBe` Just 1
    it "can compute the n = 3 case" $ do
      (fibonacci 3) `shouldBe` Just 2
    it "can compute the n = 10 case" $ do
      (fibonacci 10) `shouldBe` Just 55
    it "can compute the n = 10 case" $ do
      (fibonacci 15) `shouldBe` Just 610
    it "can't compute the n = -1 case" $ do
      (fibonacci (-1)) `shouldBe` Nothing
  describe "chebyshev1st function" $ do
    it "can compute the x = 0, n = 0 case" $ do
      (chebyshev1st 0 0) `shouldBe` Just 1
    it "can compute the x = 1000, n = 0 case" $ do
      (chebyshev1st 1000 0) `shouldBe` Just 1
    it "can compute the x = -1000, n = 0 case" $ do
      (chebyshev1st (-1000) 0) `shouldBe` Just 1
    it "can compute the x = 0, n = 1 case" $ do
      (chebyshev1st 0 1) `shouldBe` Just 0
    it "can compute the x = 1000, n = 1 case" $ do
      (chebyshev1st 1000 1) `shouldBe` Just 1000
    it "can compute the x = -1000, n = 1 case" $ do
      (chebyshev1st (-1000) 1) `shouldBe` Just (-1000)
    it "can compute the x = 0, n = 5 case" $ do
      (chebyshev1st 0 5) `shouldBe` Just 0
    it "can compute the x = 5, n = 5 case" $ do
      (chebyshev1st 5 5) `shouldBe` Just 47525
    it "can compute the x = -5, n = 5 case" $ do
      (chebyshev1st (-5) 5) `shouldBe` Just (-47525)
    it "can't compute the x = -5, n = -1 case" $ do
      (chebyshev1st (-5) (-1)) `shouldBe` Nothing
    it "can't compute the x = 10000, n = -1 case" $ do
      (chebyshev1st 10000 (-1)) `shouldBe` Nothing
    it "can't compute the x = 0, n = -1000 case" $ do
      (chebyshev1st 0 (-1000)) `shouldBe` Nothing
  describe "chebyshev2nd function" $ do
    it "can compute the x = 0, n = 0 case" $ do
      (chebyshev2nd 0 0) `shouldBe` Just 1
    it "can compute the x = 1000, n = 0 case" $ do
      (chebyshev2nd 1000 0) `shouldBe` Just 1
    it "can compute the x = -1000, n = 0 case" $ do
      (chebyshev2nd (-1000) 0) `shouldBe` Just 1
    it "can compute the x = 0, n = 1 case" $ do
      (chebyshev2nd 0 1) `shouldBe` Just 0
    it "can compute the x = 1000, n = 1 case" $ do
      (chebyshev2nd 1000 1) `shouldBe` Just 2000
    it "can compute the x = -1000, n = 1 case" $ do
      (chebyshev2nd (-1000) 1) `shouldBe` Just (-2000)
    it "can compute the x = 0, n = 4 case" $ do
      (chebyshev2nd 0 4) `shouldBe` Just 1
    it "can compute the x = 5, n = 4 case" $ do
      (chebyshev2nd 5 4) `shouldBe` Just 9701
    it "can compute the x = -5, n = 4 case" $ do
      (chebyshev2nd (-5) 4) `shouldBe` Just 9701
    it "can't compute the x = -5, n = -1 case" $ do
      (chebyshev2nd (-5) (-1)) `shouldBe` Nothing
    it "can't compute the x = 10000, n = -1 case" $ do
      (chebyshev2nd 10000 (-1)) `shouldBe` Nothing
    it "can't compute the x = 0, n = -1000 case" $ do
      (chebyshev2nd 0 (-1000)) `shouldBe` Nothing
  describe "laguerre function" $ do
    it "can compute the x = 0, n = 0 case" $ do
      (laguerre 0 0) `shouldBe` Just 1
    it "can compute the x = 1000, n = 0 case" $ do
      (laguerre 1000 0) `shouldBe` Just 1
    it "can compute the x = -1000, n = 0 case" $ do
      (laguerre (-1000) 0) `shouldBe` Just 1
    it "can compute the x = 0, n = 1 case" $ do
      (laguerre 0 1) `shouldBe` Just 1
    it "can compute the x = 1000, n = 1 case" $ do
      (laguerre 1000 1) `shouldBe` Just (-999)
    it "can compute the x = -1000, n = 1 case" $ do
      (laguerre (-1000) 1) `shouldBe` Just 1001
    it "can compute the x = 0, n = 10 case" $ do
      (laguerre 0 10) `shouldBe` Just 1
    it "can compute the x = 10, n = 10 case" $ do
      --(<1e-8) <$> (+(-1763.0)/63.0)  <$> (laguerre 10 10) `shouldBe` Just True
      floatingEqualsMaybe (laguerre 10 10) (1763.0/63.0) `shouldBe` Just True
    it "can't compute the x = -5, n = -1 case" $ do
      (laguerre (-5) (-1)) `shouldBe` Nothing
    it "can't compute the x = 10000, n = -1 case" $ do
      (laguerre 10000 (-1)) `shouldBe` Nothing
    it "can't compute the x = 0, n = -1000 case" $ do
      (laguerre 0 (-1000)) `shouldBe` Nothing
