import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Chebyshev
import Data.Maybe

signal :: [Double] -> Integer -> [(Double,Double)]
--signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]
signal xs n = [ (x,fromJust $ chebyshev1st x n) | x <- xs ]

xs = [-1,(-0.99)..1]

main = toFile def "example1_big.png" $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red, opaque orange, opaque green, opaque pink, opaque yellow]
    plot (line "1" [signal xs 1])
    plot (line "2" [signal xs 2])
    plot (line "3" [signal xs 3])
    plot (line "4" [signal xs 4])
    plot (line "15" [signal xs 15])
    plot (line "30" [signal xs 30])
