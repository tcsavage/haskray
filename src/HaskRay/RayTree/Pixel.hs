module HaskRay.RayTree.Pixel where

import HaskRay.RayTree.Sample

import HaskRay.Geometry
import HaskRay.Material
import HaskRay.Monad
import HaskRay.Vector

import qualified Control.Monad.Parallel as P

import Debug.Trace

-- | Root node of a ray tree. Branches to an arbitrary number of 'Sample's.
data Pixel = Pixel [Sample] deriving (Show, Eq)

-- | Builds a ray tree for a pixel.
tracePixel :: [Ray] -> Render Pixel
tracePixel rays = traceEvent "tracePixel" $ do
    samples <- P.mapM (traceSample 1) rays
    return $ Pixel samples

-- | Evaluates the ray tee under a pixel to determine a final 'Colour' value.
evalPixel :: Pixel -> Colour
evalPixel (Pixel samples) = traceEvent "evalPixel" $ scale (1/(fromIntegral $ length samples)) $ foldr add (Vector3 0 0 0) sampleColours
    where
        sampleColours = map (clampColour . evalSample) samples
        correct col = scale (0.25) (clampColour col)
