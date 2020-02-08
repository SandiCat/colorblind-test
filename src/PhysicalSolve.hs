{-# LANGUAGE StrictData #-}

module PhysicalSolve (showResult) where

import qualified Linear as V
import Linear ((^*),(*^))
import qualified Control.Monad.Random.Strict as R
import qualified NormalDist as Normal
import qualified Graphics.Gloss.Interface.Pure.Simulate as Gloss
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import qualified Data.List.NonEmpty as NE
import qualified Control.Parallel.Strategies as Parallel
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))

type Vec = V.V2 Float

newtype Radius = Radius Float deriving (Eq, Generic, Show)
instance NFData Radius

data Circle =
    Circle
    { center :: Vec
    , radius :: Radius
    }
    deriving (Eq, Generic, Show)
instance NFData Circle

instance Aeson.FromJSON Circle where
    parseJSON (Aeson.Object v) = Circle
        <$> (V.V2 <$> v .: "x" <*> v .: "y")
        <*> (Radius <$> v.: "r")
    parseJSON _ =
        fail "circle has to be an object"

instance Aeson.ToJSON Circle where
    toJSON (Circle (V.V2 x y) (Radius r)) =
        Aeson.object ["x" .= x, "y" .= y, "r" .= r]

newtype RelativeTo = RelativeTo Circle

repulse :: RelativeTo -> Circle -> Vec
repulse (RelativeTo c1) c2 =
    (^*)
        (V.normalize $ center c1 - center c2)
        (k / (V.distance (center c1) (center c2)) ^ 2)
  where
    k = 50000

totalRepulsion :: [Circle] -> Circle -> Vec
totalRepulsion others me =
    sum $ map (repulse $ RelativeTo me) $ filter (/= me) others

gravity :: Circle -> Vec
gravity (Circle c _) = (^*) (V.normalize $ V.negated c) (k / (V.norm c) ^ 2)
  where
    k = 10000

forceWell :: Radius -> Circle -> Vec
forceWell (Radius bigRadius) (Circle c (Radius r)) =
    ((*k) $ (^3) $ relu $ r + V.norm c - bigRadius) *^ (V.normalize $ V.negated c)
    where
        k = 10
        relu x
            | x < 0 = 0
            | otherwise = x

newtype DeltaT = DeltaT Float

deltaX :: Vec -> DeltaT -> Vec
deltaX force (DeltaT dt) =
    (0.5 * dt*dt) *^ force

step :: Radius -> DeltaT -> [Circle] -> [Circle]
step bigRadius dt circles = Parallel.parMap Parallel.rdeepseq (\c -> update (totalForce c) c) circles
  where
    -- totalForce =   totalRepulsion circles
    totalForce circle = sum [forceWell bigRadius circle, totalRepulsion circles circle, gravity circle]

    update f (Circle c r) = Circle (c + deltaX f dt) r

randomAngle :: (Floating a, R.Random a, R.MonadRandom m) => m a
randomAngle = R.getRandomR (0, 2 * pi)

randomCircle
    :: R.MonadRandom m => Normal.Mu -> Normal.Sigma -> Radius -> m Circle
randomCircle mu sigma (Radius r) = do
    rPos <- sqrt <$> R.getRandomR (0, r * r)
            -- this distributes uniformly based on area rather than radius
    phiPos <- randomAngle
    let x = rPos * cos phiPos
    let y = rPos * sin phiPos
    Circle (V.V2 x y) . Radius . realToFrac <$> Normal.getScaledNormal mu sigma

solutionPicture :: Radius -> [Circle] -> Gloss.Picture
solutionPicture (Radius bigRadius) solution =
    Gloss.pictures
        [Gloss.circle bigRadius, Gloss.pictures $ map circlePicture solution]
    where
        circlePicture (Circle (V.V2 x y) (Radius r)) =
            Gloss.circleSolid r
            & Gloss.translate x y
            & Gloss.color (Gloss.withAlpha 0.3 Gloss.black)

displayMode = (Gloss.InWindow "colorblind" (600, 400) (0, 0))

randomSolution :: R.MonadRandom m => Radius -> Int -> m [Circle]
randomSolution bigRadius n = replicateM n
        $ randomCircle (Normal.Mu 7) (Normal.Sigma 1) bigRadius

showSimulation :: IO ()
showSimulation = do
    let bigRadius = Radius 200
    initial <- randomSolution bigRadius 100
    Gloss.simulate @[Circle]
        displayMode
        Gloss.white
        30
        initial
        (solutionPicture bigRadius)
        (\_ dt model -> step bigRadius (DeltaT dt) model)

showResult :: IO ()
showResult = do
    let bigRadius = Radius 200
    initial <- randomSolution bigRadius 500
    let pic = solutionPicture bigRadius $ (NE.!! 300) $ NE.iterate (step bigRadius (DeltaT 0.01)) initial
    Gloss.display
        displayMode
        Gloss.white
        pic