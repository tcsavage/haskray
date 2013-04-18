module HaskRay
(
-- * Vectors
module HaskRay.Vector,
-- * Material
Colour,
Material(..),
Diffuse(..),
Texture,
loadTexture,
-- * Geometry
module HaskRay.Geometry,
-- * Octree
module HaskRay.Octree,
-- * Ray Tree
module HaskRay.RayTree,
-- * Projection
View(..),
-- * Settings
module HaskRay.Settings,
-- * Image Ouput
saveBMP,
makeForeignPtr,
-- * High-level Operations
render,
getPixelForest,
examineTreeAt,
-- * Scene
module HaskRay.Scene,
-- * Parser
module HaskRay.Parser
) where

import HaskRay.Vector
import HaskRay.Material
import HaskRay.Geometry
import HaskRay.Octree
import HaskRay.Out
import HaskRay.Projection
import HaskRay.Settings
import HaskRay.RayTree
import HaskRay.RayTree.String
import HaskRay.Parser
import HaskRay.Scene
import HaskRay.Monad

import qualified Control.Monad.Parallel as P

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM2)
import Data.Array.Repa.Repr.Vector
import Control.Monad
import Control.Monad.Identity

---- | Render a scene with given settings.
--render :: Settings -> Scene -> PixBuf
--render settings@(Settings w h _ rand) (Scene os view) = PixBuf (w, h) pixels
--    where
--        obs = mkObStruct os
--        sampleRays = makeCameraRays settings view -- [[Ray]]
--        pixels = runRender (P.mapM (\x -> tracePixel x >>= (return . evalPixel)) sampleRays) obs rand

-- | Render a scene with given settings.
render :: Settings -> Scene -> Array V DIM2 Colour
render settings@(Settings w h s rand gim) (Scene os view) = runIdentity $ R.computeP evaled
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view -- [[Ray]]
        traced = runRender (P.mapM tracePixel sampleRays) (obs, gim) rand
        evaled = evalPixels $ buildSampleArray (w, h, s) traced

---- | Render a scene with given settings. (Not optimised)
--renderUnOpt :: Settings -> Scene -> PixBuf
--renderUnOpt settings@(Settings w h _ rand) (Scene os view) = PixBuf (w, h) pixels
--    where
--        obs = (os, Leaf vzero 1 [], os)
--        sampleRays = makeCameraRays settings view -- [[Ray]]
--        pixels = runRender (mapM (tracePixel >=> (return . evalPixel)) sampleRays) obs rand

getPixelForest :: Settings -> Scene -> [Pixel]
getPixelForest settings@(Settings _ _ _ rand gim) (Scene os view) = forest
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view
        forest = runRender (mapM tracePixel sampleRays) (obs, gim) rand

-- | Return a textual representation of the ray tree for a given pixel (instead of rendering).
examineTreeAt :: Settings -> Scene -> (Int, Int) -> String
examineTreeAt settings@(Settings w _ _ rand gim) (Scene os view) (x, y) = treeString tree
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view
        tree = runRender (tracePixel $ sampleRays !! (y*w + x)) (obs, gim) rand
