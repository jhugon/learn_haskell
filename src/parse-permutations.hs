{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative.Permutations
import Data.Char (isAlphaNum, isDigit)
import Data.Text as T
import Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read

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
        MkConfig
          <$> toPermutation (matchConfigLine matchString "a")
          <*> toPermutation (matchConfigLine matchInt "b")
          <*> toPermutation (matchConfigLine matchFloat "c")
  runPermutation permutation

matchConfigLine :: Parser a -> T.Text -> Parser a
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

matchInt :: Parser Int
matchInt = do
  intstr <- do
    negsign <- optional $ single '-'
    numeric <- takeWhile1P (Just "digits of integer") isDigit
    pure $ case negsign of
      Just _ -> '-' `cons` numeric
      Nothing -> numeric

  case readMaybe $ T.unpack intstr of
    Just number -> pure number
    Nothing -> fail "Couldn't parse integer"

matchFloat :: Parser Float
matchFloat = do
  negsign <- optional $ single '-'
  intpart <- takeWhile1P (Just "digits of integer") isDigit
  maybedecpart <- optional $ do
    point <- single '.'
    fracpart <- takeWhileP (Just "digits of fractionalpart") isDigit
    pure $ point `cons` fracpart
  let intpartneg = case negsign of
        Just _ -> '-' `cons` intpart
        Nothing -> intpart
  let floatstr = case maybedecpart of
        Just decpart -> intpartneg <> decpart
        Nothing -> intpartneg
  case readMaybe $ T.unpack floatstr of
    Just number -> pure number
    Nothing -> fail "Couldn't parse float"

matchHyphenLine :: Parser T.Text
matchHyphenLine = do
  string "---\n"

ordereddata :: T.Text
ordereddata =
  "---\n\
  \a = aaa\n\
  \b = -235\n\
  \c = 2.35235235\n\
  \---\n"

disordereddata :: T.Text
disordereddata =
  "---\n\
  \c = -5.6\n\
  \a = aaa\n\
  \b = 2355235\n\
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
