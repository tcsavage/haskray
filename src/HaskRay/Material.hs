{-# LANGUAGE Arrows, NamedFieldPuns, FlexibleInstances, OverlappingInstances #-}

module HaskRay.Material
(
Colour,
multColour,
floatingToByte,
gammaCorrect,
Material(..),
evalMaterial,
Intersection(..),
BSDF(..),
Texture,
loadTexture,
indexTextureUV,
diffuse,
emissive,
mirror,
getIncidentRay,
traceM,
holdout,
sequenceArr,
mapArr
) where

import HaskRay.Vector
import HaskRay.Ray
import HaskRay.Monad

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))
import Control.Monad.Instances ()
import Data.Maybe
import Data.Monoid
import Control.Applicative hiding (empty)
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, Z(..), DIM2, (:.)(..))
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.IO.BMP
import Data.Word
import System.Random
import System.Random.Mersenne.Pure64

-- | Type alias for RGB colours.
type Colour = Vec3

-- | Component-wise multiplication of colours.
multColour :: Colour -> Colour -> Colour
multColour c1 c2 = (*) <$> c1 <*> c2

-- | Convert a floating point value into a byte value.
floatingToByte :: Scalar -> Word8
floatingToByte n = clampMax $ floor $ n * fromIntegral (maxBound::Word8)
    where
        clampMax :: Int -> Word8
        clampMax m
            | m > fromIntegral (maxBound::Word8) = maxBound
            | m < fromIntegral (minBound::Word8) = minBound
            | otherwise = toEnum m

-- | Apply gamma correction curve to colour values.
gammaCorrect :: Scalar -> Scalar
gammaCorrect n = n ** (1/2.2)

-- | Texture type alias.
type Texture = Array V DIM2 Colour

-- | Load a texture from a file.
loadTexture :: FilePath -> IO (Array V DIM2 Colour)
loadTexture path = do
    r <- readImageFromBMP path
    R.computeP $ R.map conv (arr r)
    where
        arr (Left err) = error $ "HaskRay.Material.loadTexture: Error loading texture: " ++ path ++ ": " ++ show err
        arr (Right x) = x
        conv (r, g, b) = (/255) <$> (fromIntegral <$> Vector3 r g b)

-- | Index a texture by UV coordinates.
indexTextureUV :: Array V DIM2 Colour -> Vec2 -> Colour
indexTextureUV tex (Vector2 u v)
    | u > 1 || v > 1 || u < 0 || v < 0 = Vector3 0 0 0 -- Default to black when out of range
    | otherwise = R.index tex (Z :. v' :. u')
    where
        (Z :. h :. w) = R.extent tex
        u' = floor $ u * fromIntegral (w-1)
        v' = floor $ v * fromIntegral (h-1)

----------------------------------
-- HERE BEGINS THE MATERIAL SYSTEM
----------------------------------

-- | Records an intersection with geometry.
data Intersection = Intersection { ipos :: !Vec3, inorm :: !Vec3, iray :: !Ray }

{-|
When perameterised over Colour, BSDFs are a record of the light reflected and transmitted from a specific point on a surface. They can however contain any other type so that more complicated computations can be performed on them.
-}
data BSDF a = BSDF { reflected :: !a, transmitted :: !a } deriving (Show, Read, Eq)

{-
BSDFs are a functor. The function is applied to the reflected pert and and transmitted part.
-}
instance Functor BSDF where
    fmap f (BSDF { reflected, transmitted }) = BSDF { reflected = f reflected, transmitted = f transmitted }

{-
BSDFs are also Applicatve functors. The function in each component is applied to it's matching component.
-}
instance Applicative BSDF where
    pure x = BSDF { reflected = x, transmitted = x }
    fs <*> xs = BSDF { reflected = reflected fs $ reflected xs, transmitted = transmitted fs $ transmitted xs }

{-
BSDFs form a Monoid under holdout and the addition of each component.
-}
instance (Vector v, Num c) => Monoid (BSDF (v c)) where
    mempty = BSDF { reflected = vzero, transmitted = vzero }
    BSDF ref1 trans1 `mappend` BSDF ref2 trans2 = BSDF { reflected = ref1 `add` ref2, transmitted = trans1 `add` trans2 }

-- | The identity BSDF.
holdout :: BSDF Colour
holdout = mempty

{-|
Materials are a description of how to calculate a final BRDF for a given incident light vector. The 'Material' type is really a material function. Material functions can be composed to produce more complicated materials. In order to track which materials emit light, there is an 'isEmissive' flag which is maintained by the primitive functions and operators. This can be queried to find the set of emissive materials to use for lighting.

A full material which can be assigned to a 'Shape' and used by the renderer must have type @Material () (BSDF Colour)@.
-}
data Material a b = Material {
    isEmissive :: !Bool,
    closure :: a -> (Ray -> Render (Maybe (Scalar, Intersection, BSDF Colour, Bool))) -> Intersection -> Vec3 -> Render b
}

-- | A more constrained version of 'closure' which only accepts full materials, and returns an action on the 'Render' monad.
evalMaterial :: Material () (BSDF a) -> (Ray -> Render (Maybe (Scalar, Intersection, BSDF Colour, Bool))) -> Intersection -> Vec3 -> Render (BSDF a)
evalMaterial mat = closure mat ()

{-
Materials are Categories with an identity function which returns the input, and a composition operator which preserves the 'isEmissive' flag.
-}
instance Category Material where
    id = Material False $ \inp _ _ _ -> return inp
    Material im1 cl1 . Material im2 cl2 = Material (im1 || im2) $ \inp trace int om_i -> do
        x1 <- cl2 inp trace int om_i 
        cl1 x1 trace int om_i

{-
Materials are arrows. They cannot be monads because tracking the 'isEmissive' flag is beyond their capabilities.
-}
instance Arrow Material where
    arr f = Material False $ \inp _ _ _ -> return $ f inp
    first (Material im cl) = Material im $ \(x1, x2) trace int om_i -> do
        r1 <- cl x1 trace int om_i
        return (r1, x2)

-- | A utility function which extracts the raw intersection value.
getIntersection :: Material () Intersection
getIntersection = Material False $ \() _ int _ -> return int

-- | A utility function which calculates the incident light ray from the intersection data and the incident light vector.
getIncidentRay :: Material () Ray
getIncidentRay = Material False $ \() _ (Intersection {ipos}) om_i -> return $ Ray ipos om_i

-- Internal use only. Get raw rendom generator.
getRandA :: Material () PureMT
getRandA = Material False $ \() _ _ _ -> getRand

-- | Generate a random value within a specified range.
randomRA :: Random r => Material (r, r) r
randomRA = Material False $ \range _ _ _ -> getRandomR range


-- Simple trace test.
--traceA :: Material (Ray, Scalar) Bool
--traceA = Material False $ \(ray, maxdist) trace _ _ rand -> (maybe False (\(dist,_,_,_) -> dist <= maxdist) $ trace ray, rand)

-- | Access to the renderer trace function. Returns Nothing if no intersection, or returns Just a BSDF if there was (in which case a recursive call back into the material system is made).
traceM :: Material Ray (Maybe (Scalar, Intersection, BSDF Colour, Bool))
traceM = Material False $ \ray trace _ _ -> trace ray

-- Version of sequence for arrows. 
sequenceArr :: Arrow a => [a b c] -> a b [c]
sequenceArr [] = arr $ const []
sequenceArr (x:xs) = proc input -> do
    x' <- x -< input
    xs' <- sequenceArr xs -< input
    returnA -< x':xs'

-- Version of mapM for arrows.
mapArr :: Arrow a => (b -> a d c) -> [b] -> a d [c]
mapArr f = sequenceArr . map f

-- Simple diffuse shading without shadows.
diffuseShading :: Material Colour (BSDF Colour)
--diffuseShading = Material False $ \col _ (Intersection {inorm}) om_i -> holdout { reflected = col } -- Flat
--diffuseShading = Material False $ \col _ (Intersection {inorm}) om_i -> holdout { reflected = fmap ((/2) . (+1)) inorm } -- Normal
--diffuseShading = Material False $ \col _ (Intersection {inorm}) om_i -> holdout { reflected = scale (max 0 (om_i `dot` inorm)) col } -- Shaded
diffuseShading = Material False fun
    where
        fun col _ (Intersection {inorm}) om_i = return $ holdout { reflected = ref }
            where
                ref = scale (max 0 (om_i `dot` inorm)) col

-- | Primitive material function.
diffuse :: Material Colour (BSDF Colour)
diffuse = proc col -> do
    out <- diffuseShading -< col                                              -- Get diffuse shading
    shad <- traceM <<< getIncidentRay -< ()                                   -- Test path to light
    returnA -< maybe holdout (\(_,_,_,e) -> if e then out else holdout) shad  -- Set BSDF to black if in shadow

{-
doLighting :: (Ray -> Maybe (Scalar, Intersection, BSDF Colour, Bool)) -> Vec3 -> Vec3 -> Render Scalar
doLighting pos om_i = do
    (obs, _) <- ask
    eps1 <- getRandomR (0, 1)
    eps2 <- getRandomR (0, 1)
    let sphere = fromMaybe (error "HaskRay.RayTree.Light.doLighting: Emissive surface not sphere.") $ cast ls
    let (Sphere center radius) = sphere
    let sw = center `sub` x
    let su = normalize (if abs (x3 sw) > 1 then Vector3 0 1 0 else Vector3 1 0 0)
    let sv = sw `cross` su
    let cosAMax = sqrt (1 - radius * radius / ((x `sub` center) `dot` (x `sub` center)))
    let cosA = 1 - eps1 + (eps1 * cosAMax)
    let sinA = sqrt $ 1 - cosA * cosA
    let phi = 2 * pi * eps2
    let l = normalize $ scale (cos phi * sinA) su `add` scale (sin phi * sinA) sv `add` scale cosA sw
    let ray = Ray x l
    let closestOb = closestIntersectObStruct ray obs
    --let getOb (Just (_,_,o)) = o
    --let closestObIsLight = if isNothing closestOb then True else (if (getOb closestOb) == light then True else False)
    let closestObIsLight = fromMaybe True $ closestOb >>= (\(_,_,o) -> Just $ o == light)
    let omega = 2*pi*(1-cosAMax)
    let fact = empow * (l `dot` norm) * omega
    return $ if closestObIsLight then clamp fact else 0
    where
        clamp fac = if fac < 0 then 0 else fac
        isReflectiveOrTransmissive (_, Intersection _ _ _ Reflective, _) = True
        isReflectiveOrTransmissive (_, Intersection _ _ _ (Transmissive _ _), _) = True
        isReflectiveOrTransmissive _ = False
-}

-- | Primitive emissive material.
emissive :: Material (Colour, Scalar) (BSDF Colour)
emissive = Material True $ \(col, power) _ _ _ -> return $ holdout { reflected = power `scale` col }

-- | Primitive reflective material.
mirror :: Material () (BSDF Colour)
--mirror = Material False fun
--    where
--        fun () trace (Intersection {ipos, inorm, iray}) _ rand = (flip (,) rand) . fromMaybe holdout $ do
--            (_, _, bsdf,_) <- trace $ Ray ipos $ (rdir iray) `sub` scale (2 * (inorm `dot` (rdir iray))) inorm
--            return bsdf
mirror = proc () -> do
    (Intersection {ipos, inorm, iray}) <- getIntersection -< ()
    traced <- traceM -< Ray ipos $ rdir iray `sub` scale (2 * (inorm `dot` rdir iray)) inorm
    returnA -< maybe holdout (\(_,_,bsdf,_) -> bsdf) traced

-- | Primitive material function which adds two BSDFs together.
addShader :: Material (BSDF Colour, BSDF Colour) (BSDF Colour)
addShader = arr $ uncurry (<>)
