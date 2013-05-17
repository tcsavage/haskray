{-|
In which the operations for generating the 'Settings' structure required by the renderer is defined.
-}
module HaskRay.Settings
(
Settings(..)
) where

import System.Random
import System.Random.Mersenne.Pure64

-- | Settings structure for renderer.
data Settings = Settings { width :: !Int, height :: !Int, samples :: !Int, randomSeed :: !PureMT, giMode :: !(Maybe Int) } deriving (Show)
