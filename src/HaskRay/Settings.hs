{-|
This module defines the operations for generating the 'Settings' structure required by the renderer.
-}
module HaskRay.Settings where

import System.Random

data Settings = Settings { width :: Int, height :: Int, samples :: Int, randomSeed :: StdGen } deriving (Show)
