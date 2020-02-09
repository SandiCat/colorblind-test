module Util where

pairs :: [a] -> [(a, a)]
pairs l = do
    x:xs <- tails l
    y <- xs
    return (x, y)
