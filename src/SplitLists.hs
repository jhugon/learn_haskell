module SplitLists
  ( splitAfter,
    splitBefore,
    splitDrop,
    splitAfterWhen,
    splitBeforeWhen,
    splitDropWhen,
    splitAfter1,
    splitBefore1,
    splitDrop1,
    splitAfterWhen1,
    splitBeforeWhen1,
    splitDropWhen1,
  )
where

-- Splits a list after each occurance of x
splitAfter :: (Eq a) => a -> [a] -> [[a]]
splitAfter x = splitAfterWhen (== x)

-- Splits a list before each occurance of x
splitBefore :: (Eq a) => a -> [a] -> [[a]]
splitBefore x = splitBeforeWhen (== x)

-- Splits a list and drops each occurance of x
splitDrop :: (Eq a) => a -> [a] -> [[a]]
splitDrop x = splitDropWhen (== x)

-- Splits a list after each occurance of predicate f
splitAfterWhen :: (a -> Bool) -> [a] -> [[a]]
splitAfterWhen _ [] = []
splitAfterWhen f l =
  case splitAfterWhen1 f l of
    ([], y) -> [y]
    (y, []) -> [y]
    (y, z) -> y : splitAfterWhen f z

-- Splits a list before each occurance of predicate f
splitBeforeWhen :: (a -> Bool) -> [a] -> [[a]]
splitBeforeWhen _ [] = []
splitBeforeWhen _ [y] = [[y]]
splitBeforeWhen f [y, z]
  | f z = [[y], [z]]
  | otherwise = [[y, z]]
splitBeforeWhen f (y : z : a : l)
  | f z = [y] : splitBeforeWhen f (z : a : l)
  | f a = [y, z] : splitBeforeWhen f (a : l)
  | otherwise = mergeWithFirstEntry [y, z] $ splitBeforeWhen f $ a : l
  where
    mergeWithFirstEntry :: [a] -> [[a]] -> [[a]]
    mergeWithFirstEntry a1 [] = [a1]
    mergeWithFirstEntry a1 (a2 : a3) = (a1 ++ a2) : a3

-- Splits a list and drops each occurance of predicate f
splitDropWhen :: (a -> Bool) -> [a] -> [[a]]
splitDropWhen _ [] = []
splitDropWhen f l =
  case splitDropWhen1 f l of
    ([], []) -> []
    ([], y) -> splitDropWhen f y
    (y, []) -> [y]
    (y, z) -> y : splitDropWhen f z

-- Splits a list after first occurance of x
splitAfter1 :: (Eq a) => a -> [a] -> ([a], [a])
splitAfter1 x = splitAfterWhen1 (== x)

-- Splits a list before first occurance of x
splitBefore1 :: (Eq a) => a -> [a] -> ([a], [a])
splitBefore1 x = splitBeforeWhen1 (== x)

-- Splits a list at first occurance of x, dropping x
splitDrop1 :: (Eq a) => a -> [a] -> ([a], [a])
splitDrop1 x = splitDropWhen1 (== x)

-- Splits a list after first occurance predicate
splitAfterWhen1 :: (a -> Bool) -> [a] -> ([a], [a])
splitAfterWhen1 _ [] = ([], [])
splitAfterWhen1 f (y : l)
  | f y = ([y], l)
  | otherwise = (y : rl, rr)
  where
    (rl, rr) = splitAfterWhen1 f l

-- Splits a list before first occurance of predicate
splitBeforeWhen1 :: (a -> Bool) -> [a] -> ([a], [a])
splitBeforeWhen1 _ [] = ([], [])
splitBeforeWhen1 f (y : l)
  | f y = ([], y : l)
  | otherwise = (y : rl, rr)
  where
    (rl, rr) = splitBeforeWhen1 f l

-- Splits a list at first occurance of predicate,
-- dropping element where predicate
splitDropWhen1 :: (a -> Bool) -> [a] -> ([a], [a])
splitDropWhen1 _ [] = ([], [])
splitDropWhen1 f (y : l)
  | f y = ([], l)
  | otherwise = (y : rl, rr)
  where
    (rl, rr) = splitDropWhen1 f l
