module Graphics where

import qualified Graphics.Gloss.Data.Picture as Gloss
import qualified Graphics.Gloss.Data.Color as Gloss
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import qualified Graphics.Gloss.Interface.IO.Animate as Gloss
import CirclePacking
import qualified Linear as V
import Persistence
import qualified Data.Aeson as Aeson

solutionPicture :: Real a => Radius a -> [Circle a] -> Gloss.Picture
solutionPicture (Radius bigRadius) solution =
    Gloss.pictures
        [ Gloss.circle (realToFrac bigRadius)
        , Gloss.pictures $ map (circlePicture . fmap realToFrac) solution]
  where
    circlePicture (Circle (V.V2 x y) (Radius r)) =
        Gloss.circleSolid r
        & Gloss.translate x y
        & Gloss.color (Gloss.withAlpha 0.3 Gloss.black)

displayMode = Gloss.InWindow "colorblind" (600, 400) (0, 0)

display :: Gloss.Picture -> IO ()
display = Gloss.display displayMode Gloss.white

displaySolution :: Real a => Radius a -> [Circle a] -> IO ()
displaySolution radius = display . solutionPicture radius

allSolutionPic :: RealFrac a => Radius a -> [[Circle a]] -> Gloss.Picture
allSolutionPic (Radius radius) solutions =
    Gloss.scale (50 / side) (50 / side)
    . Gloss.translate
        ((-0.5) * side * fromIntegral gridSide)
        ((-0.5) * side * fromIntegral gridSide)
    . Gloss.pictures
    . map (\((x,y),sol) -> Gloss.translate x y
           $ solutionPicture (Radius radius) sol)
    . zip
        [(fromIntegral i * side, fromIntegral j * side)
        | i <- [1 .. gridSide], j <- [1 .. gridSide]]
    $ solutions
  where
    side = realToFrac radius * (2 + 0.1)

    gridSide = ceiling $ sqrt $ fromIntegral $ length solutions

viewGeneration
    :: (Aeson.FromJSON a, RealFrac a) => Radius a -> FilePath -> IO ()
viewGeneration r filename =
    Gloss.animateIO
        displayMode
        Gloss.white
        (\_ -> do
             solutions <- loadSolutions filename
             return $ Graphics.allSolutionPic r solutions)
        (const $ return ())
