{-|
In which the operations for generating the 'Settings' structure required by the renderer is defined.
-}
module HaskRay.Settings
(
Settings(..)
) where

import System.Random

-- | Settings structure for renderer.
data Settings = Settings { width :: Int, height :: Int, samples :: Int, randomSeed :: StdGen } deriving (Show)
