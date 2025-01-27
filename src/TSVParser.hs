module TSVParser where

import Data.Maybe
import qualified Data.Text as T
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative ((<|>))

-- readSpacedTextData :: T.Text -> [(Float,Float)]
-- readSpacedTextData text = textToDataLine <$> linelist
--     where linelist = T.lines text

-- parseTwoNumbersLine :: String -> ReadP (Float, Float)
-- parseTwoNumbersLine txt = do
--     parses <- readP_to_S twoSpaceSeperatedNumbersLine txt
--     let noparses = 0 == (length parses)
--     if noparses then error "sldkfjasldfkjlasdf"
--     let [((x,y),"")] = parses
        
space = satisfy isSpace 
digit = satisfy isDigit
digits = many digit
digits1 = many1 digit
optionalnegsign = option "" $ string "-"

floatFractionalPart :: ReadP String
floatFractionalPart = do
    dec <- string "."
    fractionalPart <- digits1
    return $ dec ++ fractionalPart

floatIntWithDotAtEnd :: ReadP String
floatIntWithDotAtEnd = do
    digs <- digits1
    string "."
    return digs

floatJustParse :: ReadP String
floatJustParse = do
    digs <- digits
    fracpart <- floatFractionalPart
    return $ digs ++ fracpart

integer :: ReadP Int
integer = do
    negsign <- optionalnegsign
    intliteral <- digits1
    return $ read $ negsign ++ intliteral

-- use similar to python lex in https://docs.python.org/3/reference/lexical_analysis.html#floating-point-literals
floatNumber :: ReadP Float
floatNumber = do
    negsign <- optionalnegsign
    numpart <- digits1 <|> floatJustParse <|> floatIntWithDotAtEnd
    return $ read $ negsign ++ numpart

twoSpaceSeperatedNumbersLine :: ReadP (Float, Float)
twoSpaceSeperatedNumbersLine = do
    many space
    n1 <- floatNumber
    many1 space
    n2 <- floatNumber
    many space
    satisfy (== '\n')
    return (n1, n2)
