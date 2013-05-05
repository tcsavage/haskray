module HaskRay
(
-- * Vectors
module HaskRay.Vector,
-- * Material
Colour,
Material(..),
BSDF(..),
Texture,
loadTexture,
diffuse,
emissive,
mirror,
-- * Geometry
module HaskRay.Geometry,
-- * Octree
module HaskRay.Octree,
-- * Projection
View(..),
-- * Settings
module HaskRay.Settings,
-- * Image Ouput
saveBMP,
makeForeignPtr,
-- * High-level Operations
render,
-- * Scene
module HaskRay.Scene,
) where

import HaskRay.Vector
import HaskRay.Material
import HaskRay.Geometry
import HaskRay.Octree
import HaskRay.Out
import HaskRay.Projection
import HaskRay.Settings
import HaskRay.Scene
import HaskRay.Monad
import HaskRay.Ray

import qualified Control.Monad.Parallel as P

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM2, Z(..), (:.)(..))
import Data.Array.Repa.Repr.Vector
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Identity

-- | Render a scene with given settings.
render :: Settings -> Scene -> Array V DIM2 Colour
render settings@(Settings w h s rand gim) (Scene os view) = mkArray $ map (traceSamples obs) sampleRays
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view -- [[Ray]]
        mkArray ps = fromListVector (Z :. h :. w) ps

traceSamples :: ObjectStructure -> [Ray] -> Colour
traceSamples objs rs = scale (1/(fromIntegral $ length rs)) (mconcat $ map (reflected . trace objs) rs)

trace :: ObjectStructure -> Ray -> BSDF Colour
trace objs = maybe holdout (\(_, _, bsdf) -> bsdf) . traceFun objs

traceFun :: ObjectStructure -> Ray -> Maybe (Scalar, Intersection, BSDF Colour)
traceFun objs r = do
    (d, i, mat) <- closestIntersectObStruct objs r
    let bsdf = mconcat $ map ((evalMaterial mat (traceFun objs) i) . getOmega i) lights
    return $ (d, i, scale (1/(fromIntegral $ length lights)) `fmap` bsdf)
    where
        lights = filter emissiveShape $ getAll objs
        getOmega i light = center light `sub` ipos i -- TODO: Needs to be random direction
