module ChebyshevSpec where
import SpecHelper
import Chebyshev

spec :: Spec
spec = describe "Chebyshev" $ do
  context "has a function add can add two nature numbers" $ do
    (chebyshev1st 1 2) `shouldBe` 2

main :: IO ()
main = hspec spec
