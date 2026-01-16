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
    print $ splitDrop' 5 [1..4]
    print $ splitDrop' 5 []
    print $ splitDrop' 5 [1]
    print $ splitDrop' 5 [5]
    print $ splitDrop' 5 [1..10]
    print $ splitDrop' 5 [2..10]
    print $ splitDrop' 5 [3..10]
    print $ splitDrop' 5 [4..10]
    print $ splitDrop' 5 $ [1..10] ++ [1..10]
    print $ splitDrop' 5 $ [1..10] ++ [1..10] ++ [1..10]
    print $ splitDrop' 5 $ [1..10] ++ [1..4] ++ [1..10]
    print $ splitDrop' 5 $ [1..4] ++ [1..10] ++ [1..10]
    print $ splitDrop' 5 [5,5,5]
    print $ splitDrop' 5 [1..5]
    print $ splitDrop' 5 [5..10]
    print $ splitDrop' 5 [5,6,7,5,6,7]
    print $ splitDropWhen' (\x -> (x `mod` 5) == 0) [0..20]
  -- examples of using applicatives
  where 
    Just alpha = (*) <$> (Just 3) <*> (Just 4) -- evaluates to  12
    beta       = (*) <$> (Just 3) <*> Nothing -- evaluates to Nothing

-- Splits a list after each occurance of x
splitAfter' :: (Eq a) => a -> [a] -> [[a]]
splitAfter' x l = splitAfterWhen' (==x) l

-- Splits a list before each occurance of x
splitBefore' :: (Eq a) => a -> [a] -> [[a]]
splitBefore' x l = splitBeforeWhen' (==x) l

-- Splits a list and drops each occurance of x
splitDrop' :: (Eq a) => a -> [a] -> [[a]]
splitDrop' x l = splitDropWhen' (==x) l

-- Splits a list after each occurance of predicate f
splitAfterWhen' :: (a -> Bool) -> [a] -> [[a]]
splitAfterWhen' _ [] = []
splitAfterWhen' f l =
    case splitAfterWhen1' f l of
        ([],y) -> [y]
        (y,[]) -> [y]
        (y,z)  -> y:(splitAfterWhen' f z)

-- Splits a list before each occurance of predicate f
splitBeforeWhen' :: (a -> Bool) -> [a] -> [[a]]
splitBeforeWhen' _ [] = []
splitBeforeWhen' _ (y:[]) = [[y]]
splitBeforeWhen' f (y:z:[])
    | f z           = [[y],[z]]
    | otherwise        = [[y,z]]
splitBeforeWhen' f (y:z:a:l)
    | f z           = [[y]] ++ (splitBeforeWhen' f (z:a:l))
    | f a           = [[y,z]] ++ (splitBeforeWhen' f (a:l))
    | otherwise        = mergeWithFirstEntry [y,z] $ splitBeforeWhen' f $ a:l
    where
        mergeWithFirstEntry :: [a] -> [[a]] -> [[a]]
        mergeWithFirstEntry a1 [] = [a1]
        mergeWithFirstEntry a1 (a2:a3) = (a1++a2):a3

-- Splits a list and drops each occurance of predicate f
splitDropWhen' :: (a -> Bool) -> [a] -> [[a]]
splitDropWhen' _ [] = []
splitDropWhen' f l =
    case splitDropWhen1' f l of
        ([],[]) -> []
        ([],y)  -> splitDropWhen' f y
        (y,[])  -> [y]
        (y,z)   -> y:(splitDropWhen' f z)



-- Splits a list after first occurance of x
splitAfter1' :: (Eq a) => a -> [a] -> ([a],[a])
splitAfter1' x l = splitAfterWhen1' (==x) l

-- Splits a list before first occurance of x
splitBefore1' :: (Eq a) => a -> [a] -> ([a],[a])
splitBefore1' x l = splitBeforeWhen1' (==x) l

-- Splits a list at first occurance of x, dropping x
splitDrop1' :: (Eq a) => a -> [a] -> ([a],[a])
splitDrop1' x l = splitDropWhen1' (==x) l

-- Splits a list after first occurance predicate
splitAfterWhen1' :: (a -> Bool) -> [a] -> ([a],[a])
splitAfterWhen1' _ [] = ([],[])
splitAfterWhen1' f (y:l)
    | f y      = ([y],l)
    | otherwise = (y:rl,rr)
        where
            (rl,rr) = splitAfterWhen1' f l

-- Splits a list before first occurance of predicate
splitBeforeWhen1' :: (a -> Bool) -> [a] -> ([a],[a])
splitBeforeWhen1' _ [] = ([],[])
splitBeforeWhen1' f (y:l)
    | f y       = ([],y:l)
    | otherwise = (y:rl,rr)
        where
            (rl,rr) = splitBeforeWhen1' f l

-- Splits a list at first occurance of predicate,
-- dropping element where predicate
splitDropWhen1' :: (a -> Bool) -> [a] -> ([a],[a])
splitDropWhen1' _ [] = ([],[])
splitDropWhen1' f (y:l)
    | f y    = ([],l)
    | otherwise = (y:rl,rr)
        where
            (rl,rr) = splitDropWhen1' f l
