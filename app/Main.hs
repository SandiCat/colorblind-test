module Main (main) where

import qualified GeneticAlgoSolve as GA
import qualified PhysicalSolve as Physical
import Options.Applicative.Simple hiding (argument)
import OptparseApplicative.Simple.Parser
import Attoparsec.Data
import qualified Attoparsec.Data as A
import qualified CirclePacking as C
import qualified RandomExtra as R
import qualified Graphics

data Command
    = GenerateGA GA.Parameters
    | GeneratePhysical
    | ViewSolutions FilePath


main :: IO ()
main = do
    (_,runCmd) <- simpleOptions
        "0.1"
        "circle-pack"
        "Generate circle packings for colorblindness tests"
        (pure ())
        $ do
            addCommand
                "view"
                "View all solutions in a folder"
                ViewSolutions
                (argument "folder-name" (Just 'p') Nothing Nothing string)
            addCommand
                "physical"
                "Generate solutions with spring embedding"
                (const GeneratePhysical)
                (pure ())
            addCommand
                "ga"
                "Generate solutions with genetic algortihms"
                GenerateGA
                (GA.Parameters
                 <$> showableArgument
                     "num-genomes"
                     (Just 'g')
                     (Just "how many genomes in a generation")
                     (Just 50)
                     unsignedIntegral
                 <*> showableArgument
                     "num-iterations"
                     (Just 'i')
                     (Just "how many iterations to execute")
                     Nothing
                     unsignedIntegral
                 <*> showableArgument
                     "save-every"
                     (Just 's')
                     (Just "save current population every n iterations")
                     (Just 100)
                     unsignedIntegral
                 <*> (C.ProblemDef
                      <$> showableArgument
                          "outer-r"
                          (Just 'R')
                          (Just "radius of the circle that is to be packed")
                          (Just (C.Radius 200))
                          (C.Radius <$> double)
                      <*> argument
                          "mu"
                          Nothing
                          (Just "mean size of inner circle")
                          (Just (R.Mu 9, "9"))
                          (R.Mu <$> double)
                      <*> argument
                          "sigma"
                          Nothing
                          (Just "stddev of inner circle radii")
                          (Just (R.Sigma 1, "1"))
                          (R.Sigma <$> double))
                 <*> showableArgument
                     "num-circles"
                     (Just 'c')
                     (Just "how many cicles to try and pack")
                     (Just 400)
                     unsignedIntegral
                 <*> showableArgument
                     "folder-name"
                     (Just 'n')
                     (Just "name of the folder to save to")
                     Nothing
                     string
                 <*> showableArgument
                     "resume"
                     (Just 'r')
                     (Just
                          "whether to start with the population in the folder or a random one")
                     (Just False)
                     A.bool
                 <*> showableArgument
                     "multi-objective"
                     Nothing
                     (Just
                          "use multi-objective optimization")
                     (Just False)
                     A.bool
                 <*> showableArgument
                     "mutation-rate"
                     (Just 'm')
                     (Just
                          "probability of mutating a value in the genome")
                     (Just 0.001)
                     double
                 <*> showableArgument
                     "tournament-size"
                     Nothing
                     (Just
                          "size of tournaments in tournament select")
                     (Just 3)
                     unsignedIntegral
                <*> showableArgument
                     "elite-perc"
                     Nothing
                     (Just
                          "percentage of best genomes to keep each generation")
                     (Just 0.1)
                     double)
    case runCmd of
        GenerateGA params -> GA.runWithLog params
        ViewSolutions filename -> Graphics.viewGeneration (C.Radius @Double 200) filename
        GeneratePhysical -> Physical.showResult
