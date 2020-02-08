{-# LANGUAGE StrictData #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}

module GeneticAlgoSolve (logGA) where

import qualified Moo.GeneticAlgorithm.Continuous as Moo
import Data.List.Extra (chunksOf)
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import Control.Concurrent
import Control.Monad.Loops (unfoldrM,iterateM_)
import Data.List (minimum)
import qualified Linear as V
import Linear ((^*),(*^))
import Optics
import qualified Data.Aeson as Aeson
import qualified RandomExtra as R
import qualified CirclePacking as C

type Vec = V.V2 Double

type Circle = C.Circle Double

type Radius = C.Radius Double

type ProblemDef = C.ProblemDef Double

decodeCircle :: [Double] -> Maybe Circle
decodeCircle [x,y,r] = Just $ C.Circle (V.V2 x y) (C.Radius r)
decodeCircle _ = Nothing

encodeCircle :: Circle -> [Double]
encodeCircle (C.Circle (V.V2 x y) (C.Radius r)) = [x, y, r]

type Solution = [Circle]

type Genome = Moo.Genome Double

decode :: Genome -> Solution
decode = chunksOf 3 >>> map decodeCircle >>> catMaybes

encode :: Solution -> Genome
encode = concatMap encodeCircle

pairs :: [a] -> [(a, a)]
pairs l = do
    x:xs <- tails l
    y <- xs
    return (x, y)

costFunction :: Radius -> Solution -> Moo.Objective
costFunction (C.Radius bigRadius) solution =
    let outsidePenalty =
            map (\(C.Circle center (C.Radius r))
                 -> r + V.norm center - bigRadius) solution
            & filter (> 0)
            & sum
        overlapPenalty =
            pairs solution
            & filter (\(c1,c2) -> V.distance (c1 ^. C.center) (c2 ^. C.center)
                      < c1 ^. C.radius % coerced + c2 ^. C.radius % coerced)
            & map (\(c1,c2) -> (/ 2)
                   $ c1 ^. C.radius % coerced + c2 ^. C.radius % coerced
                   - V.distance (c1 ^. C.center) (c2 ^. C.center))
            & sum
        sizeReward =
            -- size is worth ~10% of overlap or outside, something like that
            solution ^.. traversed % C.radius % coerced & sum & (* 0.1)
    in outsidePenalty + overlapPenalty - sizeReward

extractStepResult :: Moo.StepResult a -> a
extractStepResult (Moo.StopGA x) = x
extractStepResult (Moo.ContinueGA x) = x

crossover :: Moo.CrossoverOp a
crossover (x:y:rest) = do
    let len = length x
    splitIx <- Moo.getRandomR (1, len - 1)
    let (x1,x2) = splitAt splitIx x
        (y1,y2) = splitAt splitIx y
    return ([x1 <> y2, y1 <> x2], rest)

step :: ProblemDef -> [Genome] -> Moo.Rand (Moo.Population Double)
step problem previousPop =
    let numGenomes = length previousPop
        elite = round $ 0.15 * fromIntegral numGenomes
        mutateProb = 0.007
    in
        -- mutateProb = 0
       fmap extractStepResult
       $ ((Moo.nextGeneration
               Moo.Minimizing
               -- ((500000 -) . costFunction (problem ^. C.outerCircle) . decode)
               (costFunction (problem ^. C.outerCircle) . decode)
               --(Moo.rouletteSelect (numGenomes - elite))
               (Moo.tournamentSelect Moo.Minimizing 5 $ numGenomes - elite)
               elite
               -- Moo.unimodalCrossoverRP
               --(Moo.blendCrossover 0.366)
               -- (Moo.simulatedBinaryCrossover 3)
               crossover
               (Moo.gaussianMutate mutateProb
                $ problem ^. C.innerCircleSigma % coerced))
                  -- pure)
              (Moo.IfObjective $ const False))
       $ Left previousPop

drawRandom :: Moo.Rand a -> IO a
drawRandom rand = Moo.evalRand rand <$> Moo.newPureMT

display :: Gloss.Picture -> IO ()
display pic =
    Gloss.display
        (Gloss.InWindow "colorblind" (600, 400) (0, 0))
        Gloss.white
        pic

displaySolution :: Radius -> Solution -> IO ()
displaySolution radius = display . C.solutionPicture radius

logGA :: IO ()
logGA = do
    let numGenomes = 50
        numIterations = 1000
        problem = C.ProblemDef (C.Radius 200) (R.Mu 9) (R.Sigma 1)
    -- initial <- map [i|data/GA/run1/#{ix}.json|] [1 .. numGenomes]
    initial <- replicateM numGenomes $ encode <$> C.randomSolution problem 400
    solutions
        <- foldlM (\currentPop i -> do
                       nextPop <- drawRandom $ step problem currentPop
                       print (i, sum $ map snd nextPop)
                       --print $ map (\y -> 500000 - y) $ map snd nextPop
                       print $ map snd nextPop
                       putStrLn "************************************++"
                       return $ map fst nextPop) initial [1 .. numIterations]
    let Just final = viaNonEmpty head solutions
    displaySolution (problem ^. C.outerCircle) $ decode final
    -- mapM_ (\(ix,solution)
    --        -> Aeson.encodeFile [i|data/GA/run1/#{ix}.json|] solution)
    --     $ zip [1 ..]
    --     $ map decode solutions
    return ()
-- generateSolution :: Radius -> IO [Solution]
-- generateSolution (C.Radius bigRadius) =
--     let numGenerations = 5 :: Int
--         numGenomes = 5 :: Int
--         elite = round $ 0.15 * fromIntegral numGenomes
--         mutateProb = 0.1
--         mutateSigma = problem
--     in map (decode . fst)
--        <$> Moo.runGA
--            (replicateM numGenomes $ encode <$> randomSolution problem)
--            (Moo.loop
--                 (Moo.Generations numGenerations)
--                 (Moo.nextGeneration
--                      Moo.Minimizing
--                      (costFunction problem . decode)
--                      (Moo.rouletteSelect (numGenomes - elite))
--                      elite
--                      Moo.unimodalCrossoverRP
--                      (Moo.gaussianMutate mutateProb mutateSigma)))
-- showGeneratedSolution :: IO ()
-- showGeneratedSolution = do
--     let problem = Problem 200 15 3
--     sol:xs <- generateSolution problem
--     display problem sol

