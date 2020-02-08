module RandomExtra where

import Data.Complex (Complex(..))
import qualified Control.Monad.Random.Strict as R
import Numeric (log)

-- | Yield two randomly selected values which follow standard
-- normal distribution.
-- code stolen from https://hackage.haskell.org/package/moo-1.2/docs/src/Moo.GeneticAlgorithm.Random.html#getNormal2
getNormal2 :: (R.Random a, RealFloat a, R.MonadRandom m) => m (a, a)
getNormal2 = do
    -- Box-Muller method
    u <- R.getRandom
    v <- R.getRandom
    let (c :+ s) = exp (0 :+ (2 * pi * v))
    let r = sqrt $ (-2) * log u
    return (r * c, r * s)

-- | Yield one randomly selected value from standard normal distribution.
-- code stolen from https://hackage.haskell.org/package/moo-1.2/docs/src/Moo.GeneticAlgorithm.Random.html#getNormal2
-- code stolen from https://hackage.haskell.org/package/moo-1.2/docs/src/Moo.GeneticAlgorithm.Random.html#getNormal2
getNormal :: (R.Random a, RealFloat a, R.MonadRandom m) => m a
getNormal = fst <$> getNormal2

newtype Mu a = Mu a

newtype Sigma a = Sigma a

getScaledNormal
    :: (R.Random a, RealFloat a, R.MonadRandom m) => Mu a -> Sigma a -> m a
getScaledNormal (Mu mu) (Sigma sigma) = (\x -> x * sigma + mu) <$> getNormal

randomAngle :: (Floating a, R.Random a, R.MonadRandom m) => m a
randomAngle = R.getRandomR (0, 2 * pi)

