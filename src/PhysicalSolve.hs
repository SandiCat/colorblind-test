{-# LANGUAGE StrictData #-}
-- these are also in package.yaml, repeated here to satisfy floskell
{-# LANGUAGE TypeApplications #-}

module PhysicalSolve where

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
import Relude.Extra.Tuple (mapBoth)
import Relude.Unsafe (fromJust)
import Util
import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import System.FilePath ((</>), (<.>))


type Vec = V.V2 Float

type Circle = C.Circle Float

type Radius = C.Radius Float

newtype RelativeTo = RelativeTo Circle

overlapping :: Circle -> Circle -> Bool
overlapping c1 c2 =
    V.distance (c1 ^. C.center) (c2 ^. C.center)
    < c1 ^. C.radius % coerced + c2 ^. C.radius % coerced

numOverlaps :: [Circle] -> Int
numOverlaps = pairs >>> map (uncurry overlapping >>> bool 1 0) >>> sum

repulse :: RelativeTo -> Circle -> Vec
repulse (RelativeTo c1) c2 =
    (^*) (V.normalize $ c1 ^. C.center - c2 ^. C.center) (k / denominator)
  where
    k = 70000

    distance = V.distance (c1 ^. C.center) (c2 ^. C.center)

    denominator =
        if overlapping c1 c2
            then distance
            else distance * distance

totalRepulsion :: [Circle] -> Circle -> Vec
totalRepulsion others me =
    sum $ map (repulse $ RelativeTo me) $ filter (/= me) others

gravity :: Circle -> Vec
gravity (C.Circle c _) = (^*) (V.normalize $ V.negated c) (k / (V.norm c) ^ 2)
  where
    k = 100000

forceWell :: Radius -> Circle -> Vec
forceWell (C.Radius bigRadius) (C.Circle c (C.Radius r)) =
    ((* k) $ (^ 5) $ relu $ r + V.norm c - bigRadius)
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

-- average :: Fractional a => [a] -> a
-- average = (/) <$> sum <*> fromIntegral . length
totalDelta :: [Circle] -> [Circle] -> Float
totalDelta prev next =
    sum $ map (uncurry V.distance . mapBoth (^. C.center)) $ zip prev next

successors :: NonEmpty a -> [(a, a)]
successors = zip <$> NE.toList <*> tail

displayMode = Gloss.InWindow "colorblind" (600, 400) (0, 0)

problem = C.ProblemDef (C.Radius 200) (R.Mu 12) (R.Sigma 1)

solve :: R.MonadRandom m => m [Circle]
solve =
    map C.shrinkCircle
    . (^. _2 % _2)
    . fromJust -- should be always safe
    . viaNonEmpty last
    . takeWhile
        (\(iter,(prev,next)) -> iter < 1000 && totalDelta prev next > 1)
    . zip [0 ..]
    . successors
    . NE.iterate (step (problem ^. C.outerCircle) (DeltaT 0.01))
    <$> C.randomSolution problem 225

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
    solution <- solve
    Gloss.display displayMode Gloss.white
        $ Graphics.solutionPicture bigRadius solution

generateLots :: Int -> FilePath -> IO ()
generateLots howMany path = Async.forConcurrently_ [1..howMany] $ \i -> do
    solution <- solve
    -- when (numOverlaps solution < 10) $ do
    when True $ do
        writeFileLBS (path </> show i <.> ".json") $ Aeson.encode solution
    	putStrLn "saved one"
    print i
