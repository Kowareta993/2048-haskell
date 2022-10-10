module Random (
    rndList,
    rndInt
) where

import System.Random
import Data.List

rndInt :: (Int, Int) -> StdGen -> (Int, StdGen)
rndInt interval = randomR interval

rndList :: Int -> (Int, Int) -> StdGen -> ([Int], StdGen)
rndList 1 interval seed = ([l], seed2)
    where
        (l, seed2) = rndInt interval seed
rndList n interval seed = (l, seed3)
    where
        (i, seed2) = rndInt interval seed
        (rest, seed3) = rndList (n-1) interval seed2
        l = [i] ++ rest

