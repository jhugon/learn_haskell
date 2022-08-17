module Chebyshev
( factorial
, chebyshev1st
, chebyshev2nd
) where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

chebyshev1st :: Double -> Integer -> Double
chebyshev1st _ 0 = 1
chebyshev1st x 1 = x
chebyshev1st x n = 2*x*(chebyshev1st x (n-1)) - (chebyshev1st x (n-2))

chebyshev2nd :: Double -> Integer -> Double
chebyshev2nd _ 0 = 1
chebyshev2nd x 1 = 2*x
chebyshev2nd x n = 2*x*(chebyshev2nd x (n-1)) - (chebyshev2nd x (n-2))

