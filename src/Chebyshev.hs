module Chebyshev
( factorial
, fibonacci
, chebyshev1st
, chebyshev2nd
, laguerre
) where

factorial :: Integer -> Maybe Integer
factorial n
    | n < 0     = Nothing
    | n == 0    = Just 1
    | otherwise = (n *) <$> factorial (n-1)

fibonacci :: Integer -> Maybe Integer
fibonacci n
    | n < 0     = Nothing
    | n == 0    = Just 0
    | n == 1    = Just 1
    | otherwise = (+) <$> fibonacci (n-1) <*> fibonacci (n-2)

chebyshev1st :: Double -> Integer -> Maybe Double
chebyshev1st x n
    | n < 0     = Nothing
    | otherwise = Just $ chebyshev1st_danger x n

chebyshev1st_danger :: Double -> Integer -> Double
chebyshev1st_danger _ 0 = 1
chebyshev1st_danger x 1 = x
chebyshev1st_danger x n = 2*x*(chebyshev1st_danger x (n-1)) - (chebyshev1st_danger x (n-2))

chebyshev2nd :: Double -> Integer -> Maybe Double
chebyshev2nd x n
    | n < 0     = Nothing
    | otherwise = Just $ chebyshev2nd_danger x n

chebyshev2nd_danger :: Double -> Integer -> Double
chebyshev2nd_danger _ 0 = 1
chebyshev2nd_danger x 1 = 2*x
chebyshev2nd_danger x n = 2*x*(chebyshev2nd_danger x (n-1)) - (chebyshev2nd_danger x (n-2))

laguerre :: Double -> Integer -> Maybe Double
laguerre x n
    | n < 0     = Nothing
    | otherwise = Just $ laguerre_danger x n

laguerre_danger :: Double -> Integer -> Double
laguerre_danger _ 0 = 1
laguerre_danger x 1 = 1 - x
laguerre_danger x n = numerator / fromInteger n
  where 
    term1 =  (2 * (fromInteger n - 1) + 1 - x) * laguerre_danger x (n-1)
    term2 = fromInteger (n-1) * laguerre_danger x (n-2)
    numerator = term1 - term2
