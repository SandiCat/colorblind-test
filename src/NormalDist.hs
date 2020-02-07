-- code stolen from https://hackage.haskell.org/package/moo-1.2/docs/src/Moo.GeneticAlgorithm.Random.html#getNormal2
module NormalDist where

import Data.Complex (Complex(..))
import qualified Control.Monad.Random.Strict as R
import Numeric (log)

-- | Yield two randomly selected values which follow standard
-- normal distribution.
getNormal2 :: R.MonadRandom m => m (Double, Double)
getNormal2 = do
    -- Box-Muller method
    u <- R.getRandom @_ @Double
    v <- R.getRandom
    let (c :+ s) = exp (0 :+ (2 * pi * v))
    let r = sqrt $ (-2) * log u
    return (r * c, r * s)

-- | Yield one randomly selected value from standard normal distribution.
getNormal :: R.MonadRandom m => m Double
getNormal = fst <$> getNormal2

newtype Mu = Mu Double
newtype Sigma = Sigma Double

getScaledNormal :: R.MonadRandom m => Mu -> Sigma -> m Double
getScaledNormal (Mu mu) (Sigma sigma) =
    (\x -> x * sigma + mu) <$> getNormal
