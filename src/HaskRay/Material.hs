{-# LANGUAGE Arrows, NamedFieldPuns, FlexibleInstances, OverlappingInstances, DeriveFunctor, DoAndIfThenElse #-}

module HaskRay.Material
(
Colour,
multColour,
floatingToByte,
gammaCorrect,
Material(..),
evalMaterial,
Intersection(..),
Scattering(..),
Texture,
loadTexture,
indexTextureUV,
diffuse,
emissive,
mirror,
transmissive,
shadeless,
showNormal,
getIncidentRay,
traceM,
holdout,
addShader,
mix,
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
import Data.Profunctor
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
When perameterised over 'Colour', scatterings are a record of the light reflected and transmitted from a specific point on a surface. They can however contain any other type so that more complicated computations can be performed on them.
-}
data Scattering a = Scattering { reflected :: !a, transmitted :: !a } deriving (Show, Read, Eq, Functor)

{-
Scatterings are also Applicatve functors. The function in each component is applied to it's matching component.
-}
instance Applicative Scattering where
    pure x = Scattering { reflected = x, transmitted = x }
    fs <*> xs = Scattering { reflected = reflected fs $ reflected xs, transmitted = transmitted fs $ transmitted xs }

{-
Scatterings form a Monoid under holdout and the addition of each component.
-}
instance (Vector v, Num c) => Monoid (Scattering (v c)) where
    mempty = pure vzero
    l `mappend` r = add <$> l <*> r

-- | The identity Scattering.
holdout :: Scattering Colour
holdout = mempty

{-|
Materials are a description of how to calculate a final BRDF for a given incident light vector. The 'Material' type is really a material function. Material functions can be composed to produce more complicated materials. In order to track which materials emit light, there is an 'isEmissive' flag which is maintained by the primitive functions and operators. This can be queried to find the set of emissive materials to use for lighting.

A full material which can be assigned to a 'Shape' and used by the renderer must have type @Material () (Scattering Colour)@.
-}
data Material a b = Material {
    isEmissive :: !Bool,
    closure :: a -> (Ray -> Render (Maybe (Scalar, Intersection, Scattering Colour, Bool))) -> Intersection -> Vec3 -> Render b
}

-- | A more constrained version of 'closure' which only accepts full materials, and returns an action on the 'Render' monad.
evalMaterial :: Material () (Scattering a) -> (Ray -> Render (Maybe (Scalar, Intersection, Scattering Colour, Bool))) -> Intersection -> Vec3 -> Render (Scattering a)
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

{-
It's useful to have conditional statements in material definitions. Hence we need an instance for ArrowChoice.
-}
instance ArrowChoice Material where
    left (Material im cl) = Material im inner
        where
            inner (Left x) trace int om_i = fmap Left $ cl x trace int om_i
            inner (Right x) _ _ _ = return $ Right x

{-
Not technically necessary but possibly useful to have.
-}
instance Functor (Material a) where
    fmap f m = arr f . m

{-
Generalisation of a "back and forth" transformation. Combines a contravariant and covariant functor.
-}
instance Profunctor Material where
    dimap f g m = arr g . m . arr f

-- | A utility function which extracts the raw intersection value.
getIntersection :: Material () Intersection
getIntersection = Material False $ \() _ int _ -> return int

-- | A utility function which calculates the incident light ray from the intersection data and the incident light vector.
getIncidentRay :: Material () Ray
getIncidentRay = Material False $ \() _ (Intersection {ipos}) om_i -> return $ Ray ipos om_i

-- | Lift a `Render` computation into a material.
liftRender :: Render a -> Material () a
liftRender a = Material False $ \() _ _ _ -> a

-- Internal use only. Get raw rendom generator.
getRandA :: Material () PureMT
getRandA = Material False $ \() _ _ _ -> getRand

-- | Generate a random value within a specified range.
randomRA :: Random r => Material (r, r) r
randomRA = Material False $ \range _ _ _ -> getRandomR range

-- Simple trace test.
--traceA :: Material (Ray, Scalar) Bool
--traceA = Material False $ \(ray, maxdist) trace _ _ rand -> (maybe False (\(dist,_,_,_) -> dist <= maxdist) $ trace ray, rand)

-- | Access to the renderer trace function. Returns Nothing if no intersection, or returns Just a Scattering if there was (in which case a recursive call back into the material system is made).
traceM :: Material Ray (Maybe (Scalar, Intersection, Scattering Colour, Bool))
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

-- | Simple material which renders a flat colour without shading or shadows.
shadeless :: Material Colour (Scattering Colour)
shadeless = arr $ \col -> holdout { reflected = col }

-- | Displays colour representation of surface normal.
showNormal :: Material () (Scattering Colour)
showNormal = proc _ -> do
    i <- getIntersection -< ()
    returnA -< holdout { reflected = fmap ((/2) . (+1)) (inorm i) }


-- Simple diffuse shading without shadows.
diffuseShading :: Material Colour (Scattering Colour)
diffuseShading = Material False fun
    where
        fun col _ (Intersection {inorm}) om_i = return $ holdout { reflected = ref }
            where
                ref = scale (max 0 (om_i `dot` inorm)) col

-- | Primitive material which renders a colour with shading and shadows.
diffuse :: Material Colour (Scattering Colour)
diffuse = proc col -> do
    out <- diffuseShading -< col                                              -- Get diffuse shading
    shad <- traceM <<< getIncidentRay -< ()                                   -- Test path to light
    returnA -< maybe holdout (\(_,_,_,e) -> if e then out else holdout) shad  -- Set Scattering to black if in shadow

-- | Primitive emissive material.
emissive :: Material (Colour, Scalar) (Scattering Colour)
emissive = Material True $ \(col, power) _ _ _ -> return $ holdout { reflected = power `scale` col }

-- | Primitive reflective material.
mirror :: Material () (Scattering Colour)
mirror = proc () -> do
    maxDepth <- liftRender atMaxDepth -< ()
    if maxDepth
    then returnA -< holdout
    else do
        (Intersection {ipos, inorm, iray}) <- getIntersection -< ()
        traced <- traceM -< Ray ipos $ rdir iray `sub` scale (2 * (inorm `dot` rdir iray)) inorm
        returnA -< maybe holdout (\(_,_,scattering,_) -> scattering) traced

--traceTransmission :: Int -> Intersection -> Object -> Render Sample
--traceTransmission depth int@(Intersection norm point (Ray _ dir) (Transmissive i m)) ob = do
--    samp <- maybe (return Background) (traceSample (depth-1)) mray2
--    ref <- traceReflection (depth-1) int
--    return $ Refraction samp ref m
--    where
--        refract nout nin norm _ dir = normalize $ (scale (nout/nin) (scale (dir `dot` norm) norm) `add` dir) `sub` scale (sqrt (1-((nout**2)*(1-(dir `dot` norm)**2)/(nin**2)))) norm
--        r1 = refract 1.0 i norm point dir
--        ray1 = Ray point r1
--        mray2 = do
--            (_, Intersection norm point (Ray _ dir) (Transmissive i _)) <- intersectOb ray1 ob
--            return $ Ray point $ refract i 1.0 norm point dir

transmissive :: Material (Scalar, Scalar) (Scattering Colour)
transmissive = proc (i, m) -> do
    (Intersection point norm (Ray _ dir)) <- getIntersection -< ()
    let refract nout nin norm _ dir = normalize $ (scale (nout/nin) (scale (dir `dot` norm) norm) `add` dir) `sub` scale (sqrt (1-((nout**2)*(1-(dir `dot` norm)**2)/(nin**2)))) norm
    let r1 = refract 1.00029 i norm point dir
    let ray1 = Ray point r1
    traced1 <- traceM -< ray1
    let mray2 = do
        (_, Intersection point norm (Ray _ dir), _, _) <- traced1
        return $ Ray point $ refract i 1.00029 norm point dir
    traced2 <- traceM -< fromJust mray2
    returnA -< maybe (pure $ pure 1) (\(_,_,scattering,_) -> scattering) traced2

-- | Primitive material function which adds two Scatterings together.
addShader :: Material (Scattering Colour, Scattering Colour) (Scattering Colour)
addShader = arr $ uncurry (<>)

mix :: Scalar -> Material (Scattering Colour, Scattering Colour) (Scattering Colour)
mix r = proc (s1, s2) -> do
    let ri = 1-r
    let s1' = (scale r) <$> s1
    let s2' = (scale ri) <$> s2
    addShader -< (s1', s2')
