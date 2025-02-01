module Main where

import TerminalPlotter
import TSVParser
import Options.Applicative

default (Int, Float)

data CmdOptions = CmdOptions { filename :: String }

cmdopts :: Parser CmdOptions
cmdopts = CmdOptions <$> argument str ( metavar "FILENAME" <> help "Input file name" )

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (cmdopts <**> helper)
            ( fullDesc
            <> progDesc "A scatter plot of each line in FILENAME, where each line is like \"1.0 5.6\""
            <> header "hsplot - a Haskell fixed-width text plotter" )

run :: CmdOptions -> IO ()
run ( CmdOptions fn ) = runScatterPlotFromFile fn
