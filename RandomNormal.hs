module RandomNormal
( randomnormal
) where

import System.Random

-- From Wikipedia Box--Muller transform

randomNormal :: RandomGen g => g -> (Double, Double, g)
randomNormal gen = (randn0,randn1,gen) where
    randn0 = r * cos(2* pi * rand2)
    randn1 = r * sin(2* pi * rand2)
    r = sqrt (-2 log rand1 )
    (rand1, _) = random gen
    (rand2, _) = random gen
