
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
    let (y1,y2) = splitAt splitIx y
    -- (x1,x2) <- splitAt splitIx <$> Moo.shuffle x
    -- (y1,y2) <- splitAt splitIx <$> Moo.shuffle y
    return ([x1 <> y2, y1 <> x2], rest)
crossover (x:rest) = return ([x], rest)
crossover [] = return ([], [])

objectives problem =
    [ --  (Moo.Maximizing, sizesReward)
      (Moo.Minimizing, overlapPenalty)
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
    toSign Moo.Maximizing = -1
    toSign Moo.Minimizing = 1

costToFitness :: (Genome -> Moo.Objective) -> (Genome -> Moo.Objective)
costToFitness f genome = 50000 - f genome -- meh :P

{- | works with negative values but very hacky -}
fixedRouletteSelection :: Moo.ProblemType -> Int -> Moo.SelectionOp a
fixedRouletteSelection maxOrMin howMany population =
    population
    & traversed % _2 %~ (case maxOrMin of
                             Moo.Maximizing -> id
                             Moo.Minimizing -> negate)
    & partsOf (traversed % _2) %~ makePositive
    & traversed % _2 %~ (+ 100)
    & Moo.rouletteSelect howMany
    <&> partsOf (traversed % _2) .~ (population ^.. traversed % _2)
  where
    makePositive l = (\x -> x - minimum l) <$> l

data Parameters =
     -- reordering fields breaks option parsing!
    Parameters
    { _numGenomes :: Int
    , _numIterations :: Int
    , _saveEvery :: Int
    , _problem :: ProblemDef
    , _circlesPerSolution :: Int
    , _path :: FilePath
    , _resume :: Bool
    , _multiObjective :: Bool
    , _mutationRate :: Double
    , _tournamentSize :: Int
    , _elitePercent :: Double
    }

makeLenses ''Parameters

stepMultiObjective
    :: Parameters -> ProblemDef -> [Genome] -> Moo.Rand (Moo.Population Double)
stepMultiObjective params problem previousPop =
    fmap extractStepResult
    $ Moo.stepNSGA2bt
        (objectives problem)
        crossover
        (Moo.gaussianMutate (params ^. mutationRate)
         $ problem ^. C.innerCircleSigma % coerced)
        (Moo.IfObjective $ const False)
    $ Left previousPop

stepSingleObjective
    :: Parameters -> ProblemDef -> [Genome] -> Moo.Rand (Moo.Population Double)
stepSingleObjective params problem previousPop =
    let numGenomes = length previousPop
        elite = round $ (params ^. elitePercent) * fromIntegral numGenomes
    in fmap extractStepResult
       $ Moo.nextGeneration
           Moo.Maximizing
           (costToFitness $ totalCost problem)
           (Moo.stochasticUniversalSampling $ numGenomes - elite)
           --    (Moo.rouletteSelect $ numGenomes - elite)
           --    (fixedRouletteSelection Moo.Minimizing $ numGenomes - elite)
           --    (Moo.tournamentSelect Moo.Minimizing (params ^. tournamentSize)
            -- $ numGenomes - elite)
           elite
           --crossover
           (Moo.blendCrossover 0.366)
           (Moo.gaussianMutate (params ^. mutationRate)
            $ problem ^. C.innerCircleSigma % coerced)
           (Moo.IfObjective $ const False)
       $ Left previousPop

drawRandom :: Moo.Rand a -> IO a
drawRandom rand = Moo.evalRand rand <$> Moo.newPureMT

runWithLog :: Parameters -> IO ()
runWithLog parameters = do
    let prob = parameters ^. problem
        step =
            if parameters ^. multiObjective
                then stepMultiObjective
                else stepSingleObjective
    initial <- if parameters ^. resume
        then map encode <$> loadSolutions (parameters ^. path)
        else replicateM (parameters ^. numGenomes)
            $ encode
            <$> C.randomSolution prob (parameters ^. circlesPerSolution)
    solutions <- foldlM
        (\currentPop currentIter -> do
             nextPop <- drawRandom $ step parameters prob currentPop
             let scores = map (objectiveValues prob . fst) nextPop
             pPrint
                 ( currentIter
                 , sum $ mconcat scores
                 , map sum $ transpose scores
                 , objectiveValues prob
                       $ minimumOn (totalCost prob)
                       $ map fst nextPop
                 , ((fromIntegral $ sum $ map (length . decode . fst) nextPop))
                       / fromIntegral (length nextPop))
             when (currentIter `mod` parameters ^. saveEvery == 0) $ do
                 saveSolutions (parameters ^. path)
                     $ map (decode . fst) nextPop
                 putStrLn "saved"
             putStrLn "************************************++"
             return $ map fst nextPop)
        initial
        [0 .. parameters ^. numIterations]
    saveSolutions (parameters ^. path) $ map decode solutions

