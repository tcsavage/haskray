{-# LANGUAGE NamedFieldPuns, OverlappingInstances, BangPatterns #-}

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

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM2, Z(..), (:.)(..))
import Data.Array.Repa.Repr.Vector
import Data.List.Split (chunksOf)
import Data.Maybe
import Control.Applicative
import Control.Monad
--import Control.Monad.Par hiding (get)
--import qualified Control.Monad.Par as P
import Control.Parallel
import Control.Parallel.Strategies

--parMapChunk :: NFData b => Int -> (a -> b) -> [a] -> Par [b]
--parMapChunk n f xs = do
--    xss <- parMap (map f) (chunksOf n xs)
--    return (concat xss)

-- | Render a scene with given settings.
--render :: Settings -> Scene -> Array V DIM2 Colour
--render settings@(Settings w h s rand gim) (Scene os view) = mkArray (evalRender (mapM (traceSamples obs) sampleRays) rand)
--    where
--        obs = mkObStruct os
--        sampleRays = makeCameraRays settings view -- [[Ray]]
--        mkArray ps = fromListVector (Z :. h :. w) ps

-- Using monad Eval
--render :: Settings -> Scene -> Array V DIM2 Colour
--render settings@(Settings w h s rand gim) (Scene os view) = mkArray $ runEval $ do
--    obs <- rpar $ mkObStruct os
--    sampleRays <- rseq $ makeCameraRays settings view  -- [[Ray]]
--    rseq obs
--    samplesM <- parList rpar $! map (traceSamples obs) sampleRays
--    samples <- rseq $ sequence samplesM
--    return (evalRender samples rand)
--    where
--        mkArray !ps = fromListVector (Z :. h :. w) ps

render :: Settings -> Scene -> Array V DIM2 Colour
render settings@(Settings w h s rand gim) (Scene os view) = mkArray $ runEval $ do
    obs <- rpar $ mkObStruct os
    sampleRays <- rpar $ chunksOf w $ makeCameraRays settings view  -- [[Ray]]
    rands <- rpar $ splitGen (length sampleRays) rand
    pairs <- rseq $ zip sampleRays rands
    samplesChunks <- parList rpar $ map (\(samples, rand') -> (evalRender $ mapM (traceSamples obs) samples) rand') pairs
    return $! (concat samplesChunks)
    where
        mkArray ps = fromListVector (Z :. h :. w) ps

-- Using monad Par
--render :: Settings -> Scene -> Array V DIM2 Colour
--render settings@(Settings w h s rand gim) (Scene os view) = mkArray $ (flip evalRender rand) $ sequence $ runPar $ do
--    parMapChunk 8 (traceSamples obs) sampleRays
--    where
--        obs = mkObStruct os
--        sampleRays = makeCameraRays settings view -- [[Ray]]
--        mkArray ps = fromListVector (Z :. h :. w) ps

splitGen :: Int -> PureMT -> [PureMT]
splitGen 0 _ = []
splitGen n rand = map pureMT $ take n $ randoms rand

traceSamples :: ObjectStructure -> [Ray] -> Render Colour
traceSamples objs !rs = do
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
        (Just (d, i@(Intersection {ipos}), mat)) -> do
            bsdfs <- forM lights $ \l -> do
                ldir <- randomSampleDir l ipos
                --let ldir = getOmega i l
                evalMaterial mat (traceFun objs) i ldir
            return $! Just (d, i, scale (1/(fromIntegral $ length lights)) `fmap` (mconcat bsdfs), isEmissive mat)
    where
        lights = filter emissiveShape $ getAll objs
        getOmega i light = normalize (center light `sub` ipos i) -- TODO: Needs to be random direction
