{-# LANGUAGE StrictData #-}
-- these are also in package.yaml, repeated here to satisfy floskell
{-# LANGUAGE TypeApplications #-}

module PhysicalSolve (showResult) where

import qualified Linear as V
import Linear ((^*),(*^))
import qualified Control.Monad.Random.Strict as R
import qualified RandomExtra as R
import qualified Graphics.Gloss.Interface.Pure.Simulate as Gloss
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import qualified Data.List.NonEmpty as NE
import qualified Control.Parallel.Strategies as Parallel
import Optics
import qualified CirclePacking as C
import qualified Graphics

type Vec = V.V2 Float

type Circle = C.Circle Float

type Radius = C.Radius Float

newtype RelativeTo = RelativeTo Circle

repulse :: RelativeTo -> Circle -> Vec
repulse (RelativeTo c1) c2 =
    (^*)
        (V.normalize $ c1 ^. C.center - c2 ^. C.center)
        (k / (V.distance (c1 ^. C.center) (c2 ^. C.center)) ^ 2)
  where
    k = 50000

totalRepulsion :: [Circle] -> Circle -> Vec
totalRepulsion others me =
    sum $ map (repulse $ RelativeTo me) $ filter (/= me) others

gravity :: Circle -> Vec
gravity (C.Circle c _) = (^*) (V.normalize $ V.negated c) (k / (V.norm c) ^ 2)
  where
    k = 10000

forceWell :: Radius -> Circle -> Vec
forceWell (C.Radius bigRadius) (C.Circle c (C.Radius r)) =
    ((* k) $ (^ 3) $ relu $ r + V.norm c - bigRadius)
    *^ (V.normalize $ V.negated c)
  where
    k = 10

    relu x
      | x < 0 = 0
      | otherwise = x

newtype DeltaT = DeltaT Float

deltaX :: Vec -> DeltaT -> Vec
deltaX force (DeltaT dt) = (0.5 * dt * dt) *^ force

step :: Radius -> DeltaT -> [Circle] -> [Circle]
step bigRadius dt circles =
    Parallel.parMap Parallel.rdeepseq (\c -> update (totalForce c) c) circles
    -- totalForce =   totalRepulsion circles

      where
        totalForce circle =
            sum
                [ forceWell bigRadius circle
                , totalRepulsion circles circle
                , gravity circle]

        update f (C.Circle c r) = C.Circle (c + deltaX f dt) r

displayMode = (Gloss.InWindow "colorblind" (600, 400) (0, 0))

problem = C.ProblemDef (C.Radius 200) (R.Mu 7) (R.Sigma 2)

showSimulation :: IO ()
showSimulation = do
    let bigRadius = problem ^. C.outerCircle
    initial <- C.randomSolution problem 100
    Gloss.simulate
        @[Circle]
        displayMode
        Gloss.white
        30
        initial
        (Graphics.solutionPicture bigRadius)
        (\_ dt model -> step bigRadius (DeltaT dt) model)

showResult :: IO ()
showResult = do
    let bigRadius = problem ^. C.outerCircle
    initial <- C.randomSolution problem 500
    let pic =
            Graphics.solutionPicture bigRadius
            $ (NE.!! 300)
            $ NE.iterate (step bigRadius (DeltaT 0.01)) initial
    Gloss.display displayMode Gloss.white pic