module Main where

import System.Random

default (Int, Float)

data Vector = Vector
  { x :: Float,
    y :: Float
  }

-- Vector sum v1 + v2
vectorSum :: Vector -> Vector -> Vector
vectorSum v1 v2 = Vector (x v1 + x v2) (y v1 + y v2)

-- Vector difference v1 - v2
vectorDiff :: Vector -> Vector -> Vector
vectorDiff v1 v2 = Vector (x v1 - x v2) (y v1 - y v2)

vectorScale :: Float -> Vector -> Vector
vectorScale sf v = Vector (sf * x v) (sf * y v)

vectorNormalized :: Vector -> Vector
vectorNormalized v = vectorScale sf v
  where
    sf = 1.0 / vectorMag v

vectorMag :: Vector -> Float
vectorMag = sqrt . vectorMag2

vectorMag2 :: Vector -> Float
vectorMag2 v = x v ** 2 + y v ** 2

instance Show Vector where
  show v =
    "("
      ++ show (x v)
      ++ ", "
      ++ show (y v)
      ++ " )"

data Body = Body
  { pos :: Vector,
    vel :: Vector,
    mass :: Float
  }

instance Show Body where
  show body =
    "Body ("
      ++ " pos "
      ++ show (pos body)
      ++ " vel "
      ++ show (vel body)
      ++ " mass "
      ++ show (mass body)
      ++ " )"

randomBody :: IO Body
randomBody = do
  Body <$> randVec <*> randVec <*> return 1.0
  where
    range = (-1.0, 1.0)
    randFloat = randomRIO range
    randVec :: IO Vector
    randVec = Vector <$> randFloat <*> randFloat

accelerationFromBody :: Float -> Body -> Body -> Vector
accelerationFromBody g self other = vectorScale magnitude direction
  where
    drvec = vectorDiff (pos other) (pos self)
    dr2 = vectorMag2 drvec
    magnitude = g * mass other / dr2
    direction = vectorNormalized drvec

-- you must exclude self from the list of "other" bodies
sumAcceleration :: Float -> Body -> [Body] -> Vector
sumAcceleration g self others = sumAccelerations $ map (accelerationFromBody g self) others
  where
    sumAccelerations :: [Vector] -> Vector
    sumAccelerations = foldr vectorSum (Vector 0.0 0.0)

makeOthersLists :: [a] -> [[a]]
makeOthersLists bodies = [others i | i <- [0 .. n - 1]]
  where
    others i = take i bodies ++ drop (i + 1) bodies
    n = length bodies

computeAccelerations :: Float -> [Body] -> [Vector]
computeAccelerations g bodies = zipWith (sumAcceleration g) bodies others
  where
    others = makeOthersLists bodies

updatePosVel :: Float -> Body -> Vector -> Body
updatePosVel dt body acceleration = body {pos = newpos, vel = newvel}
  where
    newpos :: Vector
    newpos = vectorSum (pos body) (vectorScale dt (vel body))
    newvel :: Vector
    newvel = vectorSum (vel body) (vectorScale dt acceleration)

stepSimulation :: Float -> Float -> [Body] -> [Body]
stepSimulation g dt initialBodies = zipWith (updatePosVel dt) initialBodies a
  where
    a = computeAccelerations g initialBodies

runSimulation :: Float -> Float -> [Body] -> [[Body]]
runSimulation g dt = iterate (stepSimulation g dt)

main :: IO ()
main = do
  let g = 1.0
  let n = 2
  let dt = 0.1
  let niter = 100
  initialBodies <- traverse (const randomBody) [1 .. n]
  let simulationResults = iterate (stepSimulation g dt) initialBodies
  let finiteSimulationResults = take niter simulationResults
  print $ last finiteSimulationResults

testbodies :: [Body]
testbodies = [Body {pos = Vector 0.0 0.0, vel = Vector 1.0 0.0, mass = 1.0}, Body {pos = Vector 0.0 1.0, vel = Vector (-1.0) 0.0, mass = 1.0}]
