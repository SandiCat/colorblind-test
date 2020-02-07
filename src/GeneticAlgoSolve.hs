{-# LANGUAGE StrictData #-}
{-# LANGUAGE NumericUnderscores #-}

module GeneticAlgoSolve (logGA) where

import qualified Moo.GeneticAlgorithm.Continuous as Moo
import Data.List.Extra (chunksOf)
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import Control.Concurrent
import Control.Monad.Loops (unfoldrM, iterateM_)
import Data.List (minimum)

data Circle =
    Circle
    { cX :: Double
    , cY :: Double
    , cRadius :: Double
    }

decodeCircle :: [Double] -> Maybe Circle
decodeCircle [x,y,r] = Just $ Circle x y r
decodeCircle _ = Nothing

encodeCircle :: Circle -> [Double]
encodeCircle (Circle x y r) = [x, y, r]

type Solution = [Circle]

type Genome = Moo.Genome Double

decode :: Genome -> Solution
decode = chunksOf 3 >>> map decodeCircle >>> catMaybes
{-# SCC decode #-}

encode :: Solution -> Genome
encode = concatMap encodeCircle

data Problem =
    Problem
    { pRadius :: Double
    , pMeanInner :: Double
    , pStddevInner :: Double
    }

problemCircle :: Problem -> Circle
problemCircle problem =
    Circle 0 0 $ pRadius problem

circleArea :: Double -> Double
circleArea r = r ^ 2 * pi

getNormal :: Double -> Double -> Moo.Rand Double
getNormal mean stddev = Moo.getNormal <&> \x -> x * stddev + mean

randomAngle :: (Floating a, Moo.Random a) => Moo.Rand a
randomAngle = Moo.getRandomR (0, 2 * pi)

randomCircle :: Problem -> Moo.Rand Circle
randomCircle problem = do
    rPos <- sqrt <$> Moo.getRandomR (0, pRadius problem & (^ 2))
            -- this distributes uniformly based on area rather than radius
    phiPos <- randomAngle
    let x = rPos * cos phiPos
    let y = rPos * sin phiPos
    Circle x y <$> getNormal (pMeanInner problem) (pStddevInner problem)

randomSolution :: Problem -> Moo.Rand Solution
randomSolution problem = do
    -- innerAreaSum <- (* (circleArea $ pRadius problem))
    --     <$> Moo.getRandomR (0.7, 0.9)
    let innerAreaSum = 0.9 * circleArea (pRadius problem)

            -- what portion of the enclosing circle's area to fill with small circles?
    -- generate circles until the sum of their areas exceeds innerAreaSum
    -- TODO: 
    --  find a more declarative way to express this
    --  ideally i'd have an infinite list of random circles and then use list zip, takeWhile, and scanl
    --  replicateM doesn't exist and `sequence . replicate` isn't sufficiently lazy
    --  getRandoms from MonadRandom requires a degenerate typeclass instance (getRandomR for circles?)
    --  probably requires a streaming library?
    flip unfoldrM 0 $ \currentSum -> do
        circle <- randomCircle problem
        -- look at this mess
        let area = circleArea $ cRadius circle
        let newSum = currentSum + area
        if newSum >= innerAreaSum
            then return Nothing
            else return $ Just (circle, newSum)

radToDeg :: Floating a => a -> a
radToDeg = (* (180 / pi))

degToRad :: Floating a => a -> a
degToRad = (* (pi / 180))

{- | stolen from https://hackage.haskell.org/package/colour-2.3.2/docs/src/Data-Colour-RGBSpace-HSV.html#hsv -}
hsvToColor :: Float -> Float -> Float -> Gloss.Color
hsvToColor h' s v = case hi of
    0 -> rgb v t p
    1 -> rgb q v p
    2 -> rgb p v t
    3 -> rgb p q v
    4 -> rgb t p v
    5 -> rgb v p q
  where
    h = radToDeg h'

    mod1 x =
        if pf < 0
            then pf + 1
            else pf
      where
        (_,pf) = properFraction x

    hi = floor (h / 60) `mod` 6

    f = mod1 (h / 60)

    p = v * (1 - s)

    q = v * (1 - f * s)

    t = v * (1 - (1 - f) * s)

    rgb r g b = Gloss.makeColor r g b 1

randomColor :: Moo.Rand Gloss.Color
randomColor = hsvToColor <$> randomAngle <*> pure 1 <*> pure 1

solutionPicture :: Problem -> Solution -> Moo.Rand Gloss.Picture
solutionPicture problem solution = do
    innerCircles <- forM solution $ \(Circle x y r) -> do
        color <- randomColor
        return
            $ Gloss.circleSolid (realToFrac r)
            & Gloss.translate (realToFrac x) (realToFrac y)
            & Gloss.color color
    return
        $ Gloss.pictures
            [ Gloss.circle $ realToFrac $ pRadius problem
            , Gloss.pictures innerCircles]

drawRandom :: Moo.Rand a -> IO a
drawRandom rand = Moo.evalRand rand <$> Moo.newPureMT

display :: Problem -> Solution -> IO ()
display problem solution = do
    pic <- drawRandom $ solutionPicture problem solution
    Gloss.display
        (Gloss.InWindow "colorblind" (600, 400) (0, 0))
        Gloss.white
        pic

showRandomProblem :: IO ()
showRandomProblem = do
    let problem = Problem 200 15 3
    solution <- drawRandom $ randomSolution problem
    display problem solution

distance :: Circle -> Circle -> Double
distance (Circle x1 y1 _) (Circle x2 y2 _) =
    sqrt $ dx*dx + dy*dy
    where
        dx = x1 - x2
        dy = y1 - y2

pairs :: [a] -> [(a, a)]
pairs l = do
    x:xs <- tails l
    y <- xs
    return (x, y)

costFunction :: Problem -> Solution -> Moo.Objective
costFunction problem solution =
    let
        outsidePenalty =
            map (\c -> cRadius c + distance (problemCircle problem) c - pRadius problem) solution
            & filter (> 0)
            & sum

        overlapPenalty =
            pairs solution
            & filter (\(c1, c2) -> distance c1 c2 < cRadius c1 + cRadius c2)
            & map (\(c1, c2) -> (/2) $ cRadius c1 + cRadius c2 - distance c1 c2)
            & sum
    in
        outsidePenalty + overlapPenalty
{-# SCC costFunction #-}

extractStepResult :: Moo.StepResult a -> a
extractStepResult (Moo.StopGA x) = x
extractStepResult (Moo.ContinueGA x) = x

crossover :: Moo.CrossoverOp a
crossover (x:y:rest) = do
    let len = length x
    splitIx <- Moo.getRandomR (1, len - 1)
    let (x1, x2) = splitAt splitIx x
        (y1, y2) = splitAt splitIx y
    return ([x1 <> y2, y1 <> x2], rest)

step :: Problem -> [Genome] -> Moo.Rand (Moo.Population Double)
step problem previousPop =
    let numGenomes :: Int = length previousPop
        elite = round $ 0.15 * fromIntegral numGenomes
        mutateProb = 0.007
        -- mutateProb = 0
        mutateSigma = pStddevInner problem
    in
        fmap extractStepResult
        $ ((Moo.nextGeneration
                Moo.Maximizing
                ((500000 -) . costFunction problem . decode)
                (Moo.rouletteSelect (numGenomes - elite))
                elite
                -- Moo.unimodalCrossoverRP
                -- (Moo.blendCrossover 0.366)
                -- (Moo.simulatedBinaryCrossover 3)
                crossover
                (Moo.gaussianMutate mutateProb mutateSigma))
                -- pure)
            (Moo.IfObjective $ const False))
        $ Left previousPop
{-# SCC step #-}

logGA :: IO ()
logGA = do
    let numGenomes = 50
        numIterations = 500
        problem = Problem 200 7 2
    initial <- drawRandom $ replicateM numGenomes $ encode <$> replicateM 600 (randomCircle problem)
    final:_ <- foldlM  
        (\currentPop i -> do
            nextPop <- drawRandom $ step problem currentPop
            print (i, sum $ map snd nextPop)
            print $ map snd nextPop
            putStrLn "************************************++"
            return $ map fst nextPop)
        initial
        [1..numIterations]
    display problem $ decode $ final
    return ()
{-# SCC logGA #-}


generateSolution :: Problem -> IO [Solution]
generateSolution problem =
    let numGenerations :: Int = 5
        numGenomes :: Int = 5
        elite = round $ 0.15 * fromIntegral numGenomes
        mutateProb = 0.1
        mutateSigma = pStddevInner problem
    in map (decode . fst) <$> Moo.runGA
           (replicateM numGenomes $ encode <$> randomSolution problem)
           (Moo.loop
                (Moo.Generations numGenerations)
                (Moo.nextGeneration
                     Moo.Minimizing
                     (costFunction problem . decode)
                     (Moo.rouletteSelect (numGenomes - elite))
                     elite
                     Moo.unimodalCrossoverRP
                     (Moo.gaussianMutate mutateProb mutateSigma)))

showGeneratedSolution :: IO ()
showGeneratedSolution = do
    let problem = Problem 200 15 3
    sol:xs <- generateSolution problem
    display problem sol

