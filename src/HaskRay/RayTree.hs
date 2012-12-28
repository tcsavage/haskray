module HaskRay.RayTree
(
-- ** Tree Nodes
Pixel(..),
Sample(..),
Shadow(..),

-- ** Tree Operations
tracePixel,
evalPixel
) where

import HaskRay.Geometry
import HaskRay.Vector
import HaskRay.Material
import HaskRay.Light
import HaskRay.Octree
import HaskRay.Monad

import System.Random
import Control.Monad.Random
import Data.Maybe
import Data.Typeable (cast)

-- | Root node of a ray tree. Branches to an arbitrary number of 'Sample's.
data Pixel = Pixel [Sample] deriving (Show, Eq)

-- | When a sample is traced, the object it intersects with determines its value.
data Sample = Background                       -- ^ The ray hit nothing. Evaluates to a simple background colour.
            | Diff Colour [Shadow] Sample      -- ^ Diffuse surface. Contains a base colour, a shadow ray for each light, and a global illumination sample.
            | Emm Colour                       -- ^ Emissive material.
            | Reflection Sample                -- ^ Reflective surface. Contains reflected sample.
            | Refraction Sample Sample Double  -- ^ Transmissive surface. Contains refracted sample, a reflected sample and a mix factor.
            | Dead                             -- ^ Dead end sample. Represents a reflection, refraction or global illumation sample killed by russian roulette.
            deriving (Show, Eq)

-- | Shadow ray retult.
data Shadow = Shadow Colour deriving (Show, Eq)

-- | Builds a ray tree for a pixel.
tracePixel :: [Ray] -> Render Pixel
tracePixel rays = do
	mapM (traceSample) rays >>= (return . Pixel)

-- | Evaluates the ray tee under a pixel to determine a final 'Colour' value.
evalPixel :: Pixel -> Colour
evalPixel (Pixel samples) = scale (1/(fromIntegral $ length samples)) $ foldr add (Vector3 0 0 0) sampleColours
	where
		sampleColours = map (clampColour . evalSample) samples
		correct col = scale (0.25) (clampColour col)

traceSample :: Ray -> Render Sample
traceSample ray = do
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
			return $ Diff col light Dead
		procMaterial obs (Emissive col _) i _ = return $ Emm col
		procMaterial obs (Reflective) i _ = traceReflection i
		procMaterial obs (Transmissive _ _) i ob = traceTransmission i ob
		lights obs = filter isEmissive (getObjects obs ray)
		notLight (_, (Intersection _ _ _ m)) = case m of
			(Emissive _ _) -> False
			otherwise -> True

evalSample :: Sample -> Colour
evalSample Background = Vector3 0 0 0
evalSample Dead = Vector3 1 1 1
evalSample (Diff col shadows _) = foldr (add . shadCol) (Vector3 0 0 0) shadows
	where shadCol (Shadow scol) = scale (1/pi) $ col `multColour` scol
evalSample (Emm col) = col
evalSample (Reflection sample) = evalSample sample
evalSample (Refraction trans ref mix) = t `add` r
	where
		amix = max 0 (min 1 mix)
		omix = 1-amix
		t = scale amix $ evalSample trans
		r = scale omix $ evalSample ref

traceLight :: Intersection -> Object -> Render Shadow
traceLight (Intersection norm point (Ray origin dir) (Diffuse colour)) lo@(Object light matfun) = do
	obs <- ask
	fact <- doLighting point norm lo
	return $ Shadow $ scale fact colour

traceReflection :: Intersection -> Render Sample
traceReflection (Intersection norm point (Ray _ dir) _) = do
	obs <- ask
	traceSample ray >>= (return . Reflection)
	where
		r = dir `sub` (scale (2 * (norm `dot` dir)) norm)
		ray = Ray point r

-- Trace refraction at entry only.
-- TODO: fresnel reflection
traceTransmission :: Intersection -> Object -> Render Sample
traceTransmission int@(Intersection norm point (Ray _ dir) mat@(Transmissive i m)) ob = do
	obs <- ask
	samp <- maybe (return Background) traceSample mray2
	ref <- traceReflection int
	return $ Refraction samp ref m
	where
		refract nout nin norm point dir = normalize $ (scale (nout/nin) (scale (dir `dot` norm) norm) `add` dir) `sub` (scale (sqrt (1-((nout^^2)*(1-(dir `dot` norm)^^2)/(nin^^2)))) norm)
		r1 = refract 1.0 i norm point dir
		ray1 = Ray point r1
		mray2 = do
			(_, (Intersection norm point (Ray _ dir) (Transmissive i _))) <- intersectOb ray1 ob
			return $ Ray point $ refract i 1.0 norm point dir

--traceLight :: [Object] -> Intersection -> Object -> Shadow
--traceLight scene (Intersection norm point (Ray origin dir) (Diffuse colour)) (Object light matfun) = Shadow $ scale (clamp $ lightDir `dot` norm) colour
--	where
--		lightPos = center light
--		relLightPos = lightPos `sub` point
--		distance = mag relLightPos
--		lightDir = normalize relLightPos
--		shadowRay = Ray point lightDir
--		is = filter (\(dist, _) -> dist < distance) $ catMaybes $ map (intersectOb shadowRay) scene
--		clamp fac = if fac < 0 then 0 else fac
