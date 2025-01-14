module TerminalPlotter
(
drawXAxis,
drawYAxis
) where

import Text.Printf (printf)

-- Make the X-axis line and label each end
-- The x-axis is nchar wide
drawXAxis :: Int -> Float -> Float -> Float -> [[Char]]
drawXAxis nchar xmin xmax ymin
    | nchar < 20    = error "drawXAxis: nchar must be at least 20 char wide"
    | otherwise     = [topline, bottomline]
        where
            topline = yminstr ++ "┼" ++ ['─' | _ <- [1..(nchar-2)] ] ++ "┐"
            bottomline = "         " ++ xminstr ++ spacesstr ++ xmaxstr
            xminstr = printf "%-10.3f" xmin
            xmaxstr = printf "%10.3f" xmax
            yminstr = printf "%9.3f" ymin
            spacesstr = [' ' | _ <- [1..(nchar - 20)]]

-- Make the Y-axis line and label each end
-- The y-axis is nchar high
drawYAxis :: Int -> Float -> [[Char]]
drawYAxis nchar ymax 
    | nchar < 3    = error "drawYAxis: nchar must be at least 3 char tall"
    | otherwise     = [topline] ++ (take (nchar-1) middlelines)
        where
            topline = printf "%9.3f" ymax ++ "┬"
            middlelines = repeat middleline
            middleline = [' ' | _ <- [1..9]] ++ "│"

drawAll :: [[Char]]
drawAll = drawYAxis 40 500 ++ drawXAxis 60 0 200 0

-- Prints each entry of lines to stdout
-- Newlines are inserted after each line
printLinesOfList :: [String] -> IO ()
printLinesOfList lines = sequence_ $ map putStrLn lines
