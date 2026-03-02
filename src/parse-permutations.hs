{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative.Permutations
import Data.Char (isAlphaNum)
import Data.Text as T
import Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

default (Int, Float) -- sets default literal number types; stops compiler warm and uses fixed with Int instead of arb prec Integer

type Parser = Parsec Void T.Text

data Config = MkConfig {a :: T.Text, b :: Int, c :: Float}
  deriving (Show, Eq)

matchConfig :: Parser Config
matchConfig = do
  between matchHyphenLine matchHyphenLine matchConfigLines

matchConfigLines :: Parser Config
matchConfigLines = do
  let permutation =
        (,,)
          <$> toPermutation (matchConfigLine matchString "a")
          <*> toPermutation (matchConfigLine matchString "b")
          <*> toPermutation (matchConfigLine matchString "c")
  (atext, _, _) <- runPermutation permutation
  let config = MkConfig {a = atext, b = 3, c = 3.2}
  pure config

matchConfigLine :: Parser T.Text -> T.Text -> Parser T.Text
matchConfigLine matchConfigValue variableName = do
  _ <- string variableName
  _ <- single ' '
  _ <- single '='
  _ <- single ' '
  value <- matchConfigValue
  _ <- single '\n'
  pure value

matchString :: Parser T.Text
matchString = do
  takeWhile1P (Just "alphanumeric or space characters are required for string values") $ \s -> isAlphaNum s || s == ' '

matchHyphenLine :: Parser T.Text
matchHyphenLine = do
  string "---\n"

ordereddata :: T.Text
ordereddata =
  "---\n\
  \a = aaa\n\
  \b = bbb\n\
  \c = ccc\n\
  \---\n"

disordereddata :: T.Text
disordereddata =
  "---\n\
  \c = ccc\n\
  \a = aaa\n\
  \b = bbb\n\
  \---\n"

main :: IO ()
main = do
  TIO.putStrLn ordereddata
  case parse matchConfig "ordered data" ordereddata of
    Left errorBundle -> Prelude.putStr (errorBundlePretty errorBundle)
    Right config -> print config
  TIO.putStrLn disordereddata
  case parse matchConfig "disordered data" disordereddata of
    Left errorBundle -> Prelude.putStr (errorBundlePretty errorBundle)
    Right config -> print config
