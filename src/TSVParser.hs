module TSVParser where

import Data.Char
import Data.Either
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

-- Expects text input with two numbers per line
-- They can be floats, but not in scientific notation
readSpacedTextData :: T.Text -> Either String [(Float,Float)]
readSpacedTextData text = traverse (uncurry parseAndErrorsLineNo) lineandnolist
    where
        linelisttext = T.lines text
        lineliststring = T.unpack <$> linelisttext
        lineandnolist :: [(String,Int)]
        lineandnolist = zip lineliststring [1..]
        parseAndErrorsLineNo :: String -> Int -> Either String (Float,Float)
        parseAndErrorsLineNo line iline = handleeither $ parseDealWithErrors line
            where
                handleeither (Left  x) = Left $ "On line " ++ show iline ++ ": " ++ x
                handleeither (Right x) = Right x

parseDealWithErrors :: String -> Either String (Float,Float)
parseDealWithErrors line = makeresult $ readP_to_S twoSpaceSeperatedNumbersLine line
    where
        makeresult ((parse,rest):_) = Right parse
        makeresult []               = Left $ "Couldn't parse line: '" ++ line ++ "'"

space = satisfy isSpace 
digit = satisfy isDigit
digits = many digit
digits1 = many1 digit
optionalnegsign = option "" $ string "-"

matchint :: ReadP String
matchint = do
    negsign <- optionalnegsign
    intliteral <- digits1
    return $ negsign ++ intliteral

integer :: ReadP Int
integer = read <$> matchint

-- use similar to python lex in https://docs.python.org/3/reference/lexical_analysis.html#floating-point-literals
floatNumber :: ReadP Float
floatNumber = floatJustParse <|> readint <|> floatIntWithDotAtEnd
    where
        readint = read <$> matchint
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
