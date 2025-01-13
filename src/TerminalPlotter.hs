module TerminalPlotter
(
drawXAxis
) where

import Text.Printf (printf)

-- Make the X-axis line and label each end
-- The x-axis is nchar wide
drawXAxis :: Int -> Float -> Float -> [Char]
drawXAxis nchar xmin xmax 
    | nchar < 20    = error "drawXAxis: nchar must be at least 20 char wide"
    | otherwise     = topline ++ "\n" ++ bottomline
        where
            topline = "├" ++ ['─' | _ <- [1..(nchar-2)] ] ++ "┐"
            bottomline =  xminstr ++ spacesstr ++ xmaxstr
            xminstr = printf "%-10.3f" xmin
            xmaxstr = printf "%10.3f" xmax
            spacesstr = [' ' | _ <- [1..(nchar - 20)]]

