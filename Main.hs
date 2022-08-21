module Main where

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
          where Just alpha = (*) <$> (Just 3) <*> (Just 4)
