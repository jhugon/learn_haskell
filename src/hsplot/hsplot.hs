module Main where

import TerminalPlotter
import TSVParser

default (Int, Float)

main :: IO ()
main = do
    runScatterPlotFromFile "testdata.txt"
