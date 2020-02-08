
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GeneticAlgoSolve where

import qualified Moo.GeneticAlgorithm.Continuous as Moo
import qualified Moo.GeneticAlgorithm.Multiobjective as Moo
import Data.List.Extra (chunksOf,minimumOn)
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import Control.Concurrent
import Control.Monad.Loops (unfoldrM,iterateM_)
import Data.List (minimum)
import qualified Linear as V
import Linear ((^*),(*^))
import Optics
import qualified Data.Aeson as Aeson
import qualified RandomExtra as R
import System.FilePath
import System.Directory (listDirectory)
import qualified CirclePacking as C
import Persistence

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

outsidePenalty :: Radius -> Solution -> Moo.Objective
outsidePenalty (C.Radius bigRadius) solution =
    map
        (\(C.Circle center (C.Radius r)) -> r + V.norm center - bigRadius)
        solution
    & filter (> 0)
    & sum

overlapPenalty :: Solution -> Moo.Objective
overlapPenalty solution =
    pairs solution
    & filter (\(c1,c2) -> V.distance (c1 ^. C.center) (c2 ^. C.center)
              < c1 ^. C.radius % coerced + c2 ^. C.radius % coerced)
    & map (\(c1,c2) -> (/ 2)
           $ c1 ^. C.radius % coerced + c2 ^. C.radius % coerced
           - V.distance (c1 ^. C.center) (c2 ^. C.center))
    & sum

sizesReward :: Solution -> Moo.Objective
sizesReward solution =
    solution ^.. traversed % C.radius % coerced & sum & (* 0.1)

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

objectives problem =
    [ (Moo.Maximizing, sizesReward)
    , (Moo.Minimizing, overlapPenalty)
    , (Moo.Minimizing, outsidePenalty (problem ^. C.outerCircle))]
    & over (traversed % _2) (. decode)

objectiveValues :: ProblemDef -> Genome -> [Moo.Objective]
objectiveValues problem = sequence $ objectives problem ^.. traversed % _2

totalCost :: ProblemDef -> Genome -> Moo.Objective
totalCost problem genome =
    sum
    $ map (\(maxOrMin,f) -> toSign maxOrMin * f genome)
    $ objectives problem
  where
    toSign Moo.Maximizing = 1
    toSign Moo.Minimizing = -1

step :: ProblemDef -> [Genome] -> Moo.Rand (Moo.Population Double)
step problem previousPop =
    let mutateProb = 0.007
    in fmap extractStepResult
       $ Moo.stepNSGA2bt
           (objectives problem)
           crossover
           (Moo.gaussianMutate mutateProb
            $ problem ^. C.innerCircleSigma % coerced)
           (Moo.IfObjective $ const False)
       $ Left previousPop

drawRandom :: Moo.Rand a -> IO a
drawRandom rand = Moo.evalRand rand <$> Moo.newPureMT

data Parameters =
    Parameters
    { _numGenomes :: Int
    , _numIterations :: Int
    , _saveEvery :: Int
    , _problem :: ProblemDef
    , _circlesPerSolution :: Int
    , _path :: FilePath
    , _resume :: Bool
    }

makeLenses ''Parameters

runWithLog :: Parameters -> IO ()
runWithLog parameters = do
    let prob = parameters ^. problem
    initial <- if parameters ^. resume
        then map encode <$> loadSolutions (parameters ^. path)
        else replicateM (parameters ^. numGenomes)
            $ encode
            <$> C.randomSolution prob (parameters ^. circlesPerSolution)
    solutions <- foldlM
        (\currentPop currentIter -> do
             nextPop <- drawRandom $ step prob currentPop
             let scores = map (objectiveValues prob . fst) nextPop
             pPrint
                 ( currentIter
                 , sum $ mconcat scores
                 , map sum $ transpose scores
                 , objectiveValues prob
                       $ minimumOn (totalCost prob)
                       $ map fst nextPop)
             -- pPrint scores
             when (currentIter `mod` parameters ^. saveEvery == 0) $ do
                 saveSolutions (parameters ^. path)
                     $ map (decode . fst) nextPop
                 putStrLn "saved"
             putStrLn "************************************++"
             return $ map fst nextPop)
        initial
        [1 .. parameters ^. numIterations]
    saveSolutions (parameters ^. path) $ map decode solutions

