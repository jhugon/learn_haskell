{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text

default (Int, Float) -- sets default literal number types; stops compiler warm and uses fixed with Int instead of arb prec Integer

ordereddata :: Text
ordereddata =
  "---\n\
  \a = aaa\n\
  \b = bbb\n\
  \c = ccc\n\
  \---"

disordereddata :: Text
disordereddata =
  "---\n\
  \c = ccc\n\
  \a = aaa\n\
  \b = bbb\n\
  \---"

main :: IO ()
main = do
  putStrLn $ unpack ordereddata
  putStrLn $ unpack disordereddata
