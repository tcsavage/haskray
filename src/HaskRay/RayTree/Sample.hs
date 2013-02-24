{-# LANGUAGE DoAndIfThenElse #-}

module HaskRay.RayTree.Sample
(
Sample(..),
traceSample,
evalSample
) where

import Prelude hiding (foldr)

import HaskRay.RayTree.Light

import HaskRay.Geometry
import HaskRay.Material
import HaskRay.Monad
import HaskRay.Octree
import HaskRay.Vector

import Data.Foldable

-- | When a sample is traced, the object it intersects with determines its value.
data Sample = Background                       -- ^ The ray hit nothing. Evaluates to a simple background colour.
            | Diff Colour [Shadow] Colour      -- ^ Diffuse surface. Contains a base colour, a shadow ray for each light, and a global illumination sample.
            | Emm Colour                       -- ^ Emissive material.
            | Reflection Sample                -- ^ Reflective surface. Contains reflected sample.
            | Refraction Sample Sample Double  -- ^ Transmissive surface. Contains refracted sample, a reflected sample and a mix factor.
            | Dead                             -- ^ Dead end sample. Represents a reflection, refraction or global illumation sample killed by russian roulette.
            deriving (Show, Eq)

-- | Trace sample from ray.
traceSample :: Int -> Ray -> Render Sample
traceSample depth ray = do
    obs <- ask
    procIntersection $ closestIntersectObStruct ray obs
    where
        -- Handle intersect results.
        procIntersection Nothing = return Background
        procIntersection (Just (_, i@(Intersection _ _ _ material), ob)) = procMaterial material i ob
        -- Handle different material types.
        procMaterial (Diffuse col) i _ = do
            obs <- ask
            let lights = filter isEmissive (getObjects obs ray)
            light <- mapM (traceLight i) lights
            gi <- traceGI depth i
            --let gi = Dead
            return $ Diff col light (evalSample gi)
        procMaterial (Texture tex) (Intersection norm pos ray _) ob = procMaterial mat (Intersection norm pos ray mat) ob
            where
                mat = Diffuse $ indexTextureUV tex $ mapTextureOb ob pos
        procMaterial (Emissive col _) i _ = return $ Emm col
        procMaterial (Reflective) i _ = traceReflection depth i
        procMaterial (Transmissive _ _) i ob = traceTransmission depth i ob

-- | Get the largest component of the colour.
maxComponent :: Colour -> Scalar
maxComponent = foldr max 0

-- | Recursively calculate the output colour of a sample.
evalSample :: Sample -> Colour
evalSample Background = Vector3 0 0 0
evalSample Dead = Vector3 0 0 0
evalSample (Diff col [] _) = col
evalSample (Diff col shadows gi) = globalIllumination <> foldr (add . shadCol) vzero shadows
    where
        shadCol (Shadow scol) = scale (1/pi) $ col `multColour` scol
        scaleParam = 0.2
        globalIllumination = scaleParam `scale` gi
evalSample (Emm col) = col
evalSample (Reflection sample) = evalSample sample
evalSample (Refraction trans ref mix) = t `add` r
    where
        amix = max 0 (min 1 mix)
        omix = 1-amix
        t = scale amix $ evalSample trans
        r = scale omix $ evalSample ref

{-|
The GI sample creates a new ray in a random direction from the intersection point, and samples it again (just like a pixel sample - i.e. lighting, reflection, etc).
When depth = 0, GI sampling only continues while russian roulette allows it to.
Because 'traceGI' is only called for diffuse surfaces, we can assume that's all we're going to get and therefore ignore the missing pattern warnings.
-}
traceGI :: Int -> Intersection -> Render Sample
traceGI depth (Intersection norm point ray (Diffuse colour))
    | depth == 0 = do -- Perform Russian Roulette to see if we continue
        r <- getRandomR (0, 1)
        if r < maxComp
        then traceGI 1 (Intersection norm point ray (Diffuse colour)) -- Run GI with 
        else return Dead
    | maxComp > 0 = do -- Calculate a GI sample direction and trace
        r1 <- fmap (2*pi*) $ getRandomR (0, 1)
        r2 <- fmap sqrt $ getRandomR (0, 1)
        let u = normalize $ cross (if abs (x3 norm) > 0.1 then Vector3 0 1 0 else Vector3 1 0 0) norm
        let v = cross norm u
        let d = normalize (scale (cos r1 * r2) u `add` scale (sin r1 *r2) v `add` scale (sqrt $ 1-r2) norm)
        traceSample (depth-1) $ Ray point d
    | otherwise = return Dead -- If the max component is 0, we can assume no diffuse reflection and stop GI here
    where
        maxComp = maxComponent colour
traceGI _ _ = error "HaskRay.RayTree.Sample.traceGI: Trying to trace GI for non-diffuse surface."

-- | Ideal reflection.
traceReflection :: Int -> Intersection -> Render Sample
traceReflection depth (Intersection norm point (Ray _ dir) _) = do
    samp <- traceSample (depth-1) ray
    return $ Reflection samp
    where
        r = dir `sub` scale (2 * (norm `dot` dir)) norm
        ray = Ray point r

-- Trace refraction at entry only.
-- TODO: fresnel reflection
-- | Simple refraction.
traceTransmission :: Int -> Intersection -> Object -> Render Sample
traceTransmission depth int@(Intersection norm point (Ray _ dir) (Transmissive i m)) ob = do
    samp <- maybe (return Background) (traceSample (depth-1)) mray2
    ref <- traceReflection (depth-1) int
    return $ Refraction samp ref m
    where
        refract nout nin norm _ dir = normalize $ (scale (nout/nin) (scale (dir `dot` norm) norm) `add` dir) `sub` scale (sqrt (1-((nout**2)*(1-(dir `dot` norm)**2)/(nin**2)))) norm
        r1 = refract 1.0 i norm point dir
        ray1 = Ray point r1
        mray2 = do
            (_, Intersection norm point (Ray _ dir) (Transmissive i _)) <- intersectOb ray1 ob
            return $ Ray point $ refract i 1.0 norm point dir
