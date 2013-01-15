module HaskRay
(
-- * Vectors
module HaskRay.Vector,
-- * Material
Colour(..),
Material(..),
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
-- * Scene
module HaskRay.Scene,
-- * Image Ouput
PixBuf(..),
savePpm,
saveBMP,
-- * High-level Operations
render,
renderUnOpt,
getPixelForest,
examineTreeAt,
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

-- | Render a scene with given settings.
render :: Settings -> Scene -> PixBuf
render settings@(Settings w h _ rand) (Scene os view) = PixBuf (w, h) pixels
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view -- [[Ray]]
        pixels = runRender (mapM (\x -> tracePixel x >>= (return . evalPixel)) sampleRays) obs rand

-- | Render a scene with given settings. (Not optimised)
renderUnOpt :: Settings -> Scene -> PixBuf
renderUnOpt settings@(Settings w h _ rand) (Scene os view) = PixBuf (w, h) pixels
    where
        obs = (os, Leaf vzero 1 [], os)
        sampleRays = makeCameraRays settings view -- [[Ray]]
        pixels = runRender (mapM (\x -> tracePixel x >>= (return . evalPixel)) sampleRays) obs rand

getPixelForest :: Settings -> Scene -> [Pixel]
getPixelForest settings@(Settings _ _ _ rand) (Scene os view) = forest
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view
        forest = runRender (mapM tracePixel sampleRays) obs rand

-- | Return a textual representation of the ray tree for a given pixel (instead of rendering).
examineTreeAt :: Settings -> Scene -> (Int, Int) -> String
examineTreeAt settings@(Settings w _ _ rand) (Scene os view) (x, y) = treeString tree
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view
        tree = runRender (tracePixel $ sampleRays !! (y*w + x)) obs rand
