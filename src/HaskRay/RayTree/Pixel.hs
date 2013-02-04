module HaskRay.RayTree.Pixel where

import HaskRay.RayTree.Sample

import HaskRay.Geometry
import HaskRay.Material
import HaskRay.Monad
import HaskRay.Vector

import qualified Control.Monad.Parallel as P

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, U, D, Z(..), DIM1, DIM2, DIM3, (:.)(..))
import Data.Array.Repa.Repr.Vector

import Debug.Trace

-- | Root node of a ray tree. Branches to an arbitrary number of 'Sample's.
type Pixel = [Sample]

-- | Builds a ray tree for a pixel.
tracePixel :: [Ray] -> Render Pixel
tracePixel rays = traceEvent "tracePixel" $ do
    samples <- P.mapM (traceSample 2) rays
    return samples

-- | Evaluates the ray tee under a pixel to determine a final 'Colour' value.
evalPixel :: Pixel -> Colour
evalPixel samples = traceEvent "evalPixel" $ scale (1/(fromIntegral $ length samples)) $ foldr add (Vector3 0 0 0) sampleColours
    where
        sampleColours = map (clampColour . evalSample) samples
        correct col = scale (0.25) (clampColour col)

buildSampleArray :: (Int, Int, Int) -> [[Sample]] -> Array V DIM3 Sample
buildSampleArray (w, h, s) pl = fromListVector (Z :. h :. w :. s) $ concat pl

evalPixels :: Array V DIM3 Sample -> Array D DIM2 Colour
evalPixels arr = R.traverse sampleColours shape magic
    where
        sampleColours = R.map (clampColour . evalSample) arr
        (Z :. _ :. _ :. s) = R.extent arr
        shape (Z :. h :. w :. s) = Z :. h :. w
        magic lookup (Z :. h :. w) = scale (1/(fromIntegral s)) $ foldr (\a b -> add (lookup (Z :. h :. w :. a)) b) vzero [0..s-1]
