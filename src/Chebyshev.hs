module Chebyshev
( factorial
, fibonacci
, chebyshev1st
, chebyshev2nd
, laguerre
) where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

chebyshev1st :: Double -> Integer -> Double
chebyshev1st _ 0 = 1
chebyshev1st x 1 = x
chebyshev1st x n = 2*x*(chebyshev1st x (n-1)) - (chebyshev1st x (n-2))

chebyshev2nd :: Double -> Integer -> Double
chebyshev2nd _ 0 = 1
chebyshev2nd x 1 = 2*x
chebyshev2nd x n = 2*x*(chebyshev2nd x (n-1)) - (chebyshev2nd x (n-2))

laguerre :: Double -> Integer -> Double
laguerre _ 0 = 1
laguerre x 1 = 1 - x
laguerre x n = numerator / fromInteger n
         where term1 =  (2 * (fromInteger n - 1) + 1 - x) * laguerre x (n-1)
               term2 = fromInteger (n-1) * laguerre x (n-2)
               numerator = term1 - term2

