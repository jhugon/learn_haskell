module TSVParser where

import Data.Char
import Data.Maybe
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

readSpacedTextData :: T.Text -> Maybe [(Float,Float)]
readSpacedTextData text = traverse parseDealWithErrors lineliststring
    where
        linelisttext = T.lines text
        lineliststring = T.unpack <$> linelisttext

parseDealWithErrors :: String -> Maybe (Float,Float)
parseDealWithErrors line = fst <$> tuple
        where
            parse = readP_to_S twoSpaceSeperatedNumbersLine
            parses = parse line
            tuple = listToMaybe parses

space = satisfy isSpace 
digit = satisfy isDigit
digits = many digit
digits1 = many1 digit
optionalnegsign = option "" $ string "-"

integer :: ReadP Int
integer = do
    negsign <- optionalnegsign
    intliteral <- digits1
    return $ read $ negsign ++ intliteral

-- use similar to python lex in https://docs.python.org/3/reference/lexical_analysis.html#floating-point-literals
floatNumber :: ReadP Float
floatNumber = floatJustParse <|> readint <|> floatIntWithDotAtEnd
    where
        readint = do
            negsign <- optionalnegsign
            num <- digits1
            return $ read $ negsign ++ num
        floatJustParse = do
            negsign <- optionalnegsign
            digs <- option "0" $ digits1
            fracpart <- floatFractionalPart
            return $ read $ negsign ++ digs ++ fracpart
        floatFractionalPart = do
            dec <- string "."
            fractionalPart <- digits1
            return $ dec ++ fractionalPart
        floatIntWithDotAtEnd = do
            negsign <- optionalnegsign
            digs <- digits1
            string "."
            return $ read $ negsign ++ digs

twoSpaceSeperatedNumbersLine :: ReadP (Float, Float)
twoSpaceSeperatedNumbersLine = do
    many space
    n1 <- floatNumber
    many1 space
    n2 <- floatNumber
    many space
    eof
    return (n1, n2)
