module Chebyshev
( factorial
, chebyshev1st
, chebyshev2st
) where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

chebyshev1st :: Double -> Integer -> Double
chebyshev1st _ 0 = 1
chebyshev1st x 1 = x
chebyshev1st x n = 2*x*(chebyshev1st x (n-1)) - (chebyshev1st x (n-2))

chebyshev2st :: Double -> Integer -> Double
chebyshev2st _ 0 = 1
chebyshev2st x 1 = 2*x
chebyshev2st x n = 2*x*(chebyshev2st x (n-1)) - (chebyshev2st x (n-2))

