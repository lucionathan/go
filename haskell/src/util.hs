module Util(
    getRandomNumber(floor, ceil)
) where

import System.Random
import Control.Monad (replicateM)


getRandomNumber :: Int -> Int -> Int
getRandomNumber