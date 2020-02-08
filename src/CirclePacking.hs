{-# LANGUAGE TemplateHaskell #-}

module CirclePacking where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:),(.=))
import qualified Linear as V
import qualified Control.Monad.Random.Strict as R
import qualified RandomExtra as R
import Optics

newtype Radius a =
    Radius
    { _unRadius :: a
    }
    deriving (Eq,Generic,Show,Functor)

makePrisms ''Radius

instance NFData a => NFData (Radius a)

data Circle a =
    Circle
    { _center :: V.V2 a
    , _radius :: Radius a
    }
    deriving (Eq,Generic,Show,Functor)

makeLenses ''Circle

instance NFData a => NFData (Circle a)

instance Aeson.FromJSON a => Aeson.FromJSON (Circle a) where
    parseJSON (Aeson.Object v) =
        Circle <$> (V.V2 <$> v .: "x" <*> v .: "y") <*> (Radius <$> v .: "r")
    parseJSON _ = fail "circle has to be an object"

instance Aeson.ToJSON a => Aeson.ToJSON (Circle a) where
    toJSON (Circle (V.V2 x y) (Radius r)) =
        Aeson.object ["x" .= x, "y" .= y, "r" .= r]

data ProblemDef a =
    ProblemDef
    { _outerCircle :: Radius a
    , _innerCircleMu :: R.Mu a
    , _innerCircleSigma :: R.Sigma a
    }

makeLenses ''ProblemDef

circleArea :: Floating a => Radius a -> a
circleArea (Radius r) = r ^ 2 * pi

randomCircle :: (R.MonadRandom m, RealFloat a, R.Random a)
             => ProblemDef a
             -> m (Circle a)
randomCircle problem = do
    rPos <- sqrt
        <$> R.getRandomR
            ( 0
            , problem ^. outerCircle % coerced * problem ^. outerCircle
                  % coerced)
            -- this distributes uniformly based on area rather than radius
    phiPos <- R.randomAngle
    let x = rPos * cos phiPos
    let y = rPos * sin phiPos
    Circle (V.V2 x y) . Radius
        <$> R.getScaledNormal
            (problem ^. innerCircleMu)
            (problem ^. innerCircleSigma)

randomSolution :: (R.MonadRandom m, RealFloat a, R.Random a)
               => ProblemDef a
               -> Int
               -> m [Circle a]
randomSolution problem n = replicateM n $ randomCircle problem
{-}
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
-}
