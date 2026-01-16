module Main where

default (Int, Float) -- sets default literal number types; stops compiler warm and uses fixed with Int instead of arb prec Integer

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    putStrLn "Again"
    putStrLn "Somethingelse"
    print 34234
    let y = "another string"
    let x = 234+1+234
        in print x
    let z = 452
    putStrLn y
    print z
    print alpha
    print beta
    print $ splitBefore' 5 [1..4]
    print $ splitBefore' 5 []
    print $ splitBefore' 5 [1]
    print $ splitBefore' 5 [5]
    print $ splitBefore' 5 [1..10]
    print $ splitBefore' 5 [2..10]
    print $ splitBefore' 5 [3..10]
    print $ splitBefore' 5 [4..10]
    print $ splitBefore' 5 $ [1..10] ++ [1..10]
    print $ splitBefore' 5 $ [1..10] ++ [1..10] ++ [1..10]
    print $ splitBefore' 5 $ [1..10] ++ [1..4] ++ [1..10]
    print $ splitBefore' 5 $ [1..4] ++ [1..10] ++ [1..10]
    print $ splitBefore' 5 [5,5,5]
    print $ splitBefore' 5 [5..10]
    print $ splitBefore' 5 [5,6,7,5,6,7]
    print $ splitBefore1' 5 [5,5,5]
  -- examples of using applicatives
  where 
    Just alpha = (*) <$> (Just 3) <*> (Just 4) -- evaluates to  12
    beta       = (*) <$> (Just 3) <*> Nothing -- evaluates to Nothing

-- Splits a list after each occurance of x
splitAfter' :: (Eq a) => a -> [a] -> [[a]]
splitAfter' _ [] = []
splitAfter' x l =
    case splitAfter1' x l of
        ([],y) -> [y]
        (y,[]) -> [y]
        (y,z)  -> y:(splitAfter' x z)

-- Splits a list before each occurance of x
splitBefore' :: (Eq a) => a -> [a] -> [[a]]
splitBefore' _ [] = []
splitBefore' x (y:[]) = [[y]]
splitBefore' x (y:z:[])
    | x == z           = [[y],[z]]
    | otherwise        = [[y,z]]
splitBefore' x (y:z:a:l)
    | x == z           = [[y]] ++ (splitBefore' x (z:a:l))
    | x == a           = [[y,z]] ++ (splitBefore' x (a:l))
    | otherwise        = mergeWithFirstEntry [y,z] $ splitBefore' x $ a:l
    where
        mergeWithFirstEntry :: [a] -> [[a]] -> [[a]]
        mergeWithFirstEntry a1 [] = [a1]
        mergeWithFirstEntry a1 (a2:a3) = (a1++a2):a3

-- Splits a list after first occurance of x
splitAfter1' :: (Eq a) => a -> [a] -> ([a],[a])
splitAfter1' _ [] = ([],[])
splitAfter1' x (y:l)
    | x == y    = ([y],l)
    | otherwise = (y:rl,rr)
        where
            (rl,rr) = splitAfter1' x l

-- Splits a list before first occurance of x
splitBefore1' :: (Eq a) => a -> [a] -> ([a],[a])
splitBefore1' _ [] = ([],[])
splitBefore1' x (y:l)
    | x == y    = ([],y:l)
    | otherwise = (y:rl,rr)
        where
            (rl,rr) = splitBefore1' x l

-- Splits a list at first occurance of x, dropping x
splitDrop1' :: (Eq a) => a -> [a] -> ([a],[a])
splitDrop1' _ [] = ([],[])
splitDrop1' x (y:l)
    | x == y    = ([],l)
    | otherwise = (y:rl,rr)
        where
            (rl,rr) = splitDrop1' x l
