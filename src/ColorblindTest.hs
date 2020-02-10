{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}

module ColorblindTest where

import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.IO as M
import qualified Control.Monad.Random.Strict as R
-- import qualified Graphics.Color.Model as CM
-- import qualified Graphics.Color.Space as CS
-- import qualified Graphics.Color.Space.RGB.Derived.SRGB as CS.SRGB
-- import qualified Graphics.Color.Adaptation.VonKries as CA
import qualified Graphics.ColorSpace as CS
import qualified CirclePacking as C
import qualified Linear as V
import qualified Graphics.Gloss as Gloss
import Optics
import qualified Data.Aeson as Aeson
import qualified Graphics

data ColoredCircle a =
    ColoredCircle
    { circle :: C.Circle a
    , color :: Gloss.Color
    }
    deriving (Functor)

makeLenses ''ColoredCircle

newtype Foreground =
    Foreground
    { unForeground :: Gloss.Color
    }

createTest :: forall m num r e.
           ( R.MonadRandom m
           , Floating num
           , RealFrac num
           , M.Manifest r M.Ix2 (CS.Pixel CS.Y e)
           , Bounded e
           , Integral e)
           => m Foreground
           -> m Gloss.Color
           -> M.Image r CS.Y e
           -> C.Radius num
           -> [C.Circle num]
           -> m [ColoredCircle num]
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
        pointToPixel v = M.Ix2 (gridHeight - floor (y / pixelSide)) (floor $ x / pixelSide)
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
        bgOrFg luminescence =
            if luminescence < 0.8 then
                randFg <&> unForeground
            else
                randBg
        colorOfCircle circle =
            circleToIndices circle
            & map (M.borderIndex (M.Fill $ CS.PixelY maxBound) image) -- white background
            & map (\(CS.PixelY val) -> fromIntegral (val - minBound)
                   / fromIntegral (maxBound :: e))
            & average
            & bgOrFg
    in mapM (\c -> ColoredCircle c <$> colorOfCircle c)

-- toGlossColor :: CS.ColorSpace cs i Float => CS.Color cs Float -> Gloss.Color
-- toGlossColor color = Gloss.makeColor r g b a
--   where
--     ((r,g,b), a) = CA.convert @_ @_ @_ @(CS.Alpha (CS.SRGB.SRGB)) @_ @Float color
--         & CS.toComponents


solutionPicture :: Real a => C.Radius a -> [ColoredCircle a] -> Gloss.Picture
solutionPicture (C.Radius bigRadius) solution =
    Gloss.pictures
        [ Gloss.circle (realToFrac bigRadius)
        , Gloss.pictures $ map (circlePicture . fmap realToFrac) solution]
  where
    circlePicture (ColoredCircle (C.Circle (V.V2 x y) (C.Radius r)) color) =
        Gloss.circleSolid r & Gloss.translate x y & Gloss.color color

try :: IO ()
try = do
    Just (solution :: [C.Circle Float]) <- Aeson.decode <$> readFileLBS "data/physical/run1/32.json"
    image :: M.Image M.S CS.Y Word16 <- M.readImage "assets/big_digit_1.png"
    let radius = C.Radius 200
    let randDim = R.getRandomR (0 :: Float, 1)
    let randColor = Gloss.makeColor <$> randDim <*> randDim <*> randDim <*> pure 1
    test <- solutionPicture radius <$> createTest 
        (Foreground <$> randColor)
        (return Gloss.blue)
        image
        radius
        solution
    Graphics.display test

