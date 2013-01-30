module HaskRay.RayTree.Sample where

import HaskRay.RayTree.Light

import HaskRay.Geometry
import HaskRay.Material
import HaskRay.Monad
import HaskRay.Octree
import HaskRay.Vector

import Debug.Trace

-- | When a sample is traced, the object it intersects with determines its value.
data Sample = Background                       -- ^ The ray hit nothing. Evaluates to a simple background colour.
            | Diff Colour [Shadow] Sample      -- ^ Diffuse surface. Contains a base colour, a shadow ray for each light, and a global illumination sample.
            | Emm Colour                       -- ^ Emissive material.
            | Reflection Sample                -- ^ Reflective surface. Contains reflected sample.
            | Refraction Sample Sample Double  -- ^ Transmissive surface. Contains refracted sample, a reflected sample and a mix factor.
            | Dead                             -- ^ Dead end sample. Represents a reflection, refraction or global illumation sample killed by russian roulette.
            deriving (Show, Eq)

traceSample :: Int -> Ray -> Render Sample
traceSample depth ray = traceEvent "traceSample" $ do
    obs <- ask
    let os = getObjects obs ray
    procIntersection $ closestIntersectObStruct ray obs
    where
        procIntersection Nothing = return Background
        procIntersection (Just (_, i@(Intersection norm point ray material), ob)) = do
            obs <- ask
            procMaterial obs material i ob
        procMaterial obs (Diffuse col) i _ = do
            obs <- ask
            light <- mapM (traceLight i) $ lights obs
            --gi <- traceGI depth i
            let gi = Dead
            return $ Diff col light gi
        procMaterial obs (Texture tex) (Intersection norm pos ray _) ob = procMaterial obs mat (Intersection norm pos ray mat) ob
            where
                mat = Diffuse $ indexTextureUV tex $ mapTextureOb ob pos
        procMaterial obs (Emissive col _) i _ = return $ Emm col
        procMaterial obs (Reflective) i _ = traceReflection depth i
        procMaterial obs (Transmissive _ _) i ob = traceTransmission depth i ob
        lights obs = filter isEmissive (getObjects obs ray)
        notLight (_, (Intersection _ _ _ m)) = case m of
            (Emissive _ _) -> False
            otherwise -> True

evalSample :: Sample -> Colour
evalSample Background = Vector3 0 0 0
evalSample Dead = Vector3 0 0 0
evalSample (Diff col [] _) = col
evalSample (Diff col shadows gi) = add (evalSample gi) $ foldr (add . shadCol) (Vector3 0 0 0) shadows
    where shadCol (Shadow scol) = scale (1/pi) $ col `multColour` scol
evalSample (Emm col) = col
evalSample (Reflection sample) = evalSample sample
evalSample (Refraction trans ref mix) = t `add` r
    where
        amix = max 0 (min 1 mix)
        omix = 1-amix
        t = scale amix $ evalSample trans
        r = scale omix $ evalSample ref

traceGI :: Int -> Intersection -> Render Sample
traceGI 0 _ = return Dead
traceGI depth int@(Intersection norm point _ _) = do
    r1 <- fmap (2*pi*) $ getRandomR (0, 1)
    r2 <- fmap sqrt $ getRandomR (0, 1)
    let w = norm
    let u = normalize $ cross (if abs (x3 w) > 0.1 then Vector3 0 1 0 else Vector3 1 0 0) w
    let v = cross w u
    let d = normalize $ ((scale (cos r1 * r2) u) `add` (scale (sin r1 *r2) v) `add` (scale (sqrt $ 1-r2) w))
    traceSample (depth-1) $ Ray point d

traceReflection :: Int -> Intersection -> Render Sample
traceReflection depth (Intersection norm point (Ray _ dir) _) = traceEvent "traceReflection" $ do
    obs <- ask
    traceSample (depth-1) ray >>= (return . Reflection)
    where
        r = dir `sub` (scale (2 * (norm `dot` dir)) norm)
        ray = Ray point r

-- Trace refraction at entry only.
-- TODO: fresnel reflection
traceTransmission :: Int -> Intersection -> Object -> Render Sample
traceTransmission depth int@(Intersection norm point (Ray _ dir) mat@(Transmissive i m)) ob = traceEvent "traceTransmission" $ do
    obs <- ask
    samp <- maybe (return Background) (traceSample (depth-1)) mray2
    ref <- traceReflection (depth-1) int
    return $ Refraction samp ref m
    where
        refract nout nin norm point dir = normalize $ (scale (nout/nin) (scale (dir `dot` norm) norm) `add` dir) `sub` (scale (sqrt (1-((nout^^2)*(1-(dir `dot` norm)^^2)/(nin^^2)))) norm)
        r1 = refract 1.0 i norm point dir
        ray1 = Ray point r1
        mray2 = do
            (_, (Intersection norm point (Ray _ dir) (Transmissive i _))) <- intersectOb ray1 ob
            return $ Ray point $ refract i 1.0 norm point dir
