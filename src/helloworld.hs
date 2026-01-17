module Main where

import SplitLists

default (Int, Float) -- sets default literal number types; stops compiler warm and uses fixed with Int instead of arb prec Integer

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "Again"
  putStrLn "Somethingelse"
  print 34234
  let y = "another string"
  let x = 234 + 1 + 234
   in print x
  let z = 452
  putStrLn y
  print z
  print alpha
  print beta
  print $ splitBefore 5 $ [1 .. 4] ++ [1 .. 10] ++ [1 .. 10]
  print $ splitBefore 5 [5, 5, 5]
  print $ splitBefore 5 [1 .. 5]
  print $ splitBefore 5 [5 .. 10]
  print $ splitBefore 5 [5, 6, 7, 5, 6, 7]
  print $ splitBeforeWhen (\x -> (x `mod` 5) == 0) [0 .. 20]
  where
    -- examples of using applicatives
    Just alpha = (*) <$> Just 3 <*> Just 4 -- evaluates to  12
    beta = (*) <$> Just 3 <*> Nothing -- evaluates to Nothing
