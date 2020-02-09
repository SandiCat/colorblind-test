{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}

module ColorblindTest where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as M
import qualified Control.Monad.Random.Strict as R
import qualified Graphics.Color.Model as Color
import qualified Graphics.Color.Space as Color
import qualified Graphics.Color.Adaptation.VonKries as Color
import qualified Graphics.ColorSpace as CS
import qualified CirclePacking as C
import qualified Linear as V
import qualified Graphics.Gloss as Gloss
import Optics

data ColoredCircle cs e a =
    ColoredCircle
    { circle :: C.Circle a
    , color :: Color.Color cs e
    }

makeLenses ''ColoredCircle

newtype Foreground cs e =
    Foreground
    { unForeground :: Color.Color cs e
    }

createTest :: forall m num r e cs.
           ( R.MonadRandom m
           , Floating num
           , RealFrac num
           , M.Manifest r M.Ix2 (CS.Pixel CS.Y e)
           , Bounded e
           , Integral e)
           => m (Foreground cs e)
           -> m (Color.Color cs e)
           -> M.Image r CS.Y e
           -> C.Radius num
           -> [C.Circle num]
           -> m [ColoredCircle cs e num]
createTest randFg randBg image (C.Radius bigRadius) =
    let
        -- grid is referring to the pixels of the image
        M.Sz2 gridHeight gridWidth = M.size image
        -- inscribe the image's rectangle inside the circle, with width 'a' and height 'b'
        bSquared =
            4 * bigRadius ^ 2
            / (1
               + ((fromIntegral gridWidth) ^ 2) / (fromIntegral gridHeight) ^ 2)
        aSquared = 4 * bigRadius ^ 2 - bSquared
        [a,b] = sqrt <$> [aSquared, bSquared]
        topLeft = V.V2 (-a / 2) (-b / 2)
        pixelSide = a / fromIntegral gridWidth
        pointToPixel v = M.Ix2 (floor $ y / pixelSide) (floor $ x / pixelSide)
          where
            V.V2 x y = v - topLeft
        -- the gloss coordinate system has y pointing up
        circleToIndices (C.Circle center (C.Radius r)) =
            [M.Ix2 i j | i <- [left .. right], j <- [bottom .. top]]
          where
            M.Ix2 left _ = pointToPixel $ center + V.V2 (-r) 0

            M.Ix2 right _ = pointToPixel $ center + V.V2 r 0

            M.Ix2 _ bottom = pointToPixel $ center + V.V2 0 (-r)

            M.Ix2 _ top = pointToPixel $ center + V.V2 0 r
        average l = sum l / fromIntegral (length l)
        pickColor circle =
            circleToIndices circle
            & map (M.borderIndex M.Edge image)
            & map (\(CS.PixelY val) -> fromIntegral val
                   / fromIntegral (maxBound :: e))
            & average
            & (> 0.5)
            & bool (randFg <&> unForeground) randBg
    in mapM (\c -> ColoredCircle c <$> pickColor c)

-- toGlossColor :: Color.ColorSpace Color.RGB i Float => Color.Color cs Float -> Gloss.Color
-- toGlossColor color = Gloss.makeColor r g b 255
--   where
--     (r,g,b) = Color.convert @_ @_ @_ @(Color.RGB) @_ @Float color
--         & Color.toComponents

solutionPicture
    :: Real a => C.Radius a -> [ColoredCircle cs e a] -> Gloss.Picture
solutionPicture (C.Radius bigRadius) solution =
    Gloss.pictures
        [ Gloss.circle (realToFrac bigRadius)
        , Gloss.pictures $ map (circlePicture . fmap realToFrac) solution]
  where
    circlePicture (C.Circle (V.V2 x y) (C.Radius r)) =
        Gloss.circleSolid r
        & Gloss.translate x y
        & Gloss.color (Gloss.withAlpha 0.3 Gloss.black)
