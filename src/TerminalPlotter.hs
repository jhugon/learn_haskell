module TerminalPlotter
(
drawXAxis,
drawYAxis
) where

import Data.Maybe
import Text.Printf (printf)
import qualified System.Console.Terminal.Size as TermSize

-- Draw a scatter plot of the given data points to the terminal
scatterPlot :: [(Float,Float)] -> IO ()
scatterPlot points = do
    (width, height) <- termSizeUnpacked
    printLinesOfList $ scatter width height points

-- Draw a scatter plot
scatter :: Int -> Int -> [(Float,Float)] -> [[Char]]
scatter width height points = [ya ++ pd | (ya,pd) <- zip yaxis plotdata] ++ xaxis
    where
        plotdata = drawPlotData datawidth dataheight points
        xaxis = drawXAxis (datawidth+1) xmin xmax ymin
        yaxis = drawYAxis dataheight ymax
        datawidth = width - 10
        dataheight = height - 3 -- axis line, labels line, and prompt line
        xmax = maximum xs
        ymax = maximum ys
        xmin = minimum xs
        ymin = minimum ys
        xs = map fst points
        ys = map snd points

-- Plot data points given in a list of the form 
-- [(x1,y1),(x2,y2),...]
-- width and height are the size of the axes data area in chars
drawPlotData :: Int -> Int -> [(Float, Float)] -> [[Char]]
drawPlotData width height points = drawLine <$> (reverse [0..(height-1)])
    where
        drawLine n = drawChar (linexs n) <$> [0..(width-1)]
        linexs n = [fst point | point <- pointsAxes, n == snd point]
        drawChar xs i = if i `elem` xs then '●' else ' '
        pointsAxes = coordsDataToAxes width height points

-- Convert from data coordinates to axes coordinates
coordsDataToAxes :: Int -> Int -> [(Float, Float)] -> [(Int, Int)]
coordsDataToAxes width height points = zip xsaxes ysaxes
    where
        xsaxes = [floor $ (x-xmin)*widthflt/xwidth | x <- xs]
        ysaxes = [floor $ (y-ymin)*heightflt/ywidth | y <- ys]
        xwidth = xmax-xmin
        ywidth = ymax-ymin
        xmax = maximum xs
        ymax = maximum ys
        xmin = minimum xs
        ymin = minimum ys
        xs = map fst points
        ys = map snd points
        widthflt = fromIntegral width-1
        heightflt = fromIntegral height-1

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

-- terminal size as IO (width, height)
termSizeUnpacked :: IO (Int, Int)
termSizeUnpacked = do
    window <- fromJust <$> TermSize.size
    let width = TermSize.width window
    let height = TermSize.height window
    return (width, height)
