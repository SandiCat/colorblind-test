module Color where

import qualified Graphics.Gloss.Data.Color as Gloss

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
