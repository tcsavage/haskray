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
getInidentRay,
traceM,
holdout,
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
render settings@(Settings w h s rand gim) (Scene os view) = mkArray $ evalRender (mapM (traceSamples obs) sampleRays) rand
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view -- [[Ray]]
        mkArray ps = fromListVector (Z :. h :. w) ps

traceSamples :: ObjectStructure -> [Ray] -> Render Colour
traceSamples objs rs = do
    colours <- mapM (fmap reflected . trace objs) rs
    return $ scale (1/(fromIntegral $ length rs)) (mconcat colours)

trace :: ObjectStructure -> Ray -> Render (BSDF Colour)
trace objs ray = do
    traced <- traceFun objs ray
    return $ maybe holdout (\(_, _, bsdf, _) -> bsdf) traced

traceFun :: ObjectStructure -> Ray -> Render (Maybe (Scalar, Intersection, BSDF Colour, Bool))
traceFun objs r = do
    rand <- get
    case closestIntersectObStruct objs r of 
        Nothing -> return Nothing
        (Just (d, i, mat)) -> do
            bsdf <- mconcat <$> mapM ((evalMaterial mat (traceFun objs) i) . getOmega i) lights
            return $ Just (d, i, scale (1/(fromIntegral $ length lights)) `fmap` bsdf, isEmissive mat)
    where
        lights = filter emissiveShape $ getAll objs
        getOmega i light = normalize (center light `sub` ipos i) -- TODO: Needs to be random direction
