{-# LANGUAGE NamedFieldPuns, OverlappingInstances, BangPatterns #-}

{-|
This module exports the high-level types and functions required to create front-end components like file I/O, user interfaces, materials and shapes. A selection of basic shapes and materials are included here. The geometry, material and vector modules are also exported individually.
-}

module HaskRay
(
-- * High-level Operations
render,
-- * Settings
module HaskRay.Settings,
-- * Geometry
module HaskRay.Geometry,
-- * Material
Colour,
Material(..),
BSDF(..),
Texture,
loadTexture,
diffuse,
emissive,
mirror,
shadeless,
showNormal,
getIncidentRay,
traceM,
holdout,
-- * Vectors
module HaskRay.Vector,
-- * Image Ouput
saveBMP,
makeForeignPtr,
-- * Scene
Scene(..),
View(..)
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

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM2, Z(..), (:.)(..))
import Data.Array.Repa.Repr.Vector
import qualified Data.Foldable as F
import Data.List.Split (chunksOf)
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies

-- | Given some parameter and scene data, will produce an image in the form of a repa array.
render :: Settings -> Scene -> Array V DIM2 Colour
render settings@(Settings w h s rand gim) (Scene os view) = mkArray $ runEval $ do
    obs <- rpar $ mkObStruct os
    sampleRays <- rpar $ chunksOf w $ makeCameraRays settings view  -- [[Ray]]
    rands <- rpar $ splitGen (length sampleRays) rand
    pairs <- rseq $ zip sampleRays rands
    samplesChunks <- parList rpar $ map (\(samples, rand') -> (evalRender $ mapM (traceSamples obs) samples) rand') pairs
    rseq (concat samplesChunks)
    where
        mkArray = fromListVector (Z :. h :. w)

-- Take a random generator and split it across several states.
splitGen :: Int -> PureMT -> [RState]
splitGen 0 _ = []
splitGen n rand = map (\r -> RState (pureMT r) 1) $ take n $ randoms rand

-- Trace all the samples of a pixel and collect the results.
traceSamples :: ObjectStructure -> [Ray] -> Render Colour
traceSamples objs !rs = do
    colours <- mapM (fmap reflected . trace objs) rs
    return $ scale (1/fromIntegral (length rs)) (mconcat colours)

-- Trace a single sample.
trace :: ObjectStructure -> Ray -> Render (BSDF Colour)
trace objs ray = do
    traced <- traceFun objs ray
    return $ maybe holdout (\(_, _, bsdf, _) -> bsdf) traced

-- The core tracing function. Passed to material system for recursive calls.
traceFun :: ObjectStructure -> Ray -> Render (Maybe (Scalar, Intersection, BSDF Colour, Bool))
traceFun objs r = do
    case closestIntersectObStruct objs r of 
        Nothing -> return Nothing
        (Just (d, i@(Intersection {ipos}), mat)) -> do
            bsdfs <- forM lights $ \l -> do
                ldir <- randomSampleDir l ipos
                --let ldir = getOmega i l
                evalMaterial mat (traceFun objs) i ldir
            let direct = mconcat bsdfs
            --gi <- withReducedDepth (return holdout) $ getGI objs i (reflected direct)
            return $! Just (d, i, scale (1/fromIntegral (length lights)) `fmap` (direct), isEmissive mat)
    where
        lights = filter emissiveShape $ getAll objs
        getOmega i light = normalize (center light `sub` ipos i) -- TODO: Needs to be random direction

-- Calculate the indirect lighting component.
getGI :: ObjectStructure -> Intersection -> Colour -> Render (BSDF Colour)
getGI objs i@(Intersection {ipos, inorm}) direct
    | True || maxComponent > 0 = do
        r <- getRandomR (0, 1)
        case r > maxComponent of
            True -> do
                ray <- getRandomRay i
                gi <- trace objs ray
                return $ BSDF (scale 0.2) (const vzero) <*> gi
            False -> return identity
    | otherwise = return identity
    where
        maxComponent = F.foldr max 0 direct
        identity = pure $ pure 1


-- Generate a random direction (for global illumination sample).
getRandomRay :: Intersection -> Render Ray
getRandomRay (Intersection {ipos, inorm}) = do
    r1 <- fmap (2*pi*) $ getRandomR (0, 1)
    r2 <- fmap sqrt $ getRandomR (0, 1)
    let u = normalize $ cross (if abs (x3 inorm) > 0.1 then Vector3 0 1 0 else Vector3 1 0 0) inorm
    let v = cross inorm u
    let d = normalize (scale (cos r1 * r2) u `add` scale (sin r1 *r2) v `add` scale (sqrt $ 1-r2) inorm)
    return $! Ray ipos d
