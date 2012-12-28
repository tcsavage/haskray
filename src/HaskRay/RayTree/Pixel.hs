module HaskRay.RayTree.Pixel where

import HaskRay.RayTree.Sample

import HaskRay.Geometry
import HaskRay.Material
import HaskRay.Monad
import HaskRay.Vector

import Control.Parallel.Strategies

-- | Root node of a ray tree. Branches to an arbitrary number of 'Sample's.
data Pixel = Pixel [Sample] deriving (Show, Eq)

-- | Builds a ray tree for a pixel.
tracePixel :: [Ray] -> Render Pixel
tracePixel rays = do
    samples <- sequence (map traceSample rays `using` parList rseq)
    return $ Pixel samples

-- | Evaluates the ray tee under a pixel to determine a final 'Colour' value.
evalPixel :: Pixel -> Colour
evalPixel (Pixel samples) = scale (1/(fromIntegral $ length samples)) $ foldr add (Vector3 0 0 0) sampleColours
    where
        sampleColours = map (clampColour . evalSample) samples
        correct col = scale (0.25) (clampColour col)
