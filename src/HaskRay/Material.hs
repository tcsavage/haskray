{-# LANGUAGE Arrows, NamedFieldPuns, KindSignatures, FlexibleInstances, OverlappingInstances #-}

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
getInidentRay,
traceM,
holdout,
sequenceArr,
mapArr
) where

import HaskRay.Vector
import HaskRay.Ray
import HaskRay.Monad

import Control.Category
import Control.Arrow
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

-- | Records an intersection with geometry.
data Intersection = Intersection { ipos :: !Vec3, inorm :: !Vec3, iray :: !Ray }

data BSDF a = BSDF { reflected :: !a, transmitted :: !a } deriving (Show, Read, Eq)

instance Functor BSDF where
    fmap f (BSDF { reflected, transmitted }) = BSDF { reflected = f reflected, transmitted = f transmitted }

instance Applicative BSDF where
    pure x = BSDF { reflected = x, transmitted = x }
    fs <*> xs = BSDF { reflected = reflected fs $ reflected xs, transmitted = transmitted fs $ transmitted xs }

instance (Vector v, Num c) => Monoid (BSDF (v c)) where
    mempty = BSDF { reflected = vzero, transmitted = vzero }
    BSDF ref1 trans1 `mappend` BSDF ref2 trans2 = BSDF { reflected = ref1 `add` ref2, transmitted = trans1 `add` trans2 }

holdout :: BSDF Colour
holdout = mempty

data Material a b = Material {
    isEmissive :: !Bool,
    closure :: !(a -> (Ray -> Render (Maybe (Scalar, Intersection, BSDF Colour, Bool))) -> Intersection -> Vec3 -> StdGen -> (b, StdGen))
}

evalMaterial :: Material () (BSDF a) -> (Ray -> Render (Maybe (Scalar, Intersection, BSDF Colour, Bool))) -> Intersection -> Vec3 -> Render (BSDF a)
evalMaterial mat trace int om_i = do
    rand <- get
    let (result, rand') = closure mat () trace int om_i rand
    put rand'
    return result

instance Category Material where
    id = Material False $ \inp _ _ _ rand -> (inp, rand)
    Material im1 cl1 . Material im2 cl2 = Material (im1 || im2) $ \inp trace int om_i rand -> let (x1, rand') = cl2 inp trace int om_i rand in cl1 x1 trace int om_i rand'

instance Arrow Material where
    arr f = Material False $ \inp _ _ _ rand -> (f inp, rand)
    first (Material im cl) = Material im $ \(x1, x2) trace int om_i rand -> let (r1, rand') = cl x1 trace int om_i rand in ((r1, x2), rand')

getIntersection :: Material () Intersection
getIntersection = Material False $ \() _ int _ rand -> (int, rand)

getInidentRay :: Material () Ray
getInidentRay = Material False $ \() _ (Intersection {ipos}) om_i rand -> (Ray ipos om_i, rand)

-- | Internal use only.
getRand :: Material () StdGen
getRand = Material False $ \() _ _ _ rand -> (rand, rand)

randomRA :: Random r => Material (r, r) r
randomRA = Material False $ \range _ _ _ rand -> randomR range rand


-- Simple trace test.
--traceA :: Material (Ray, Scalar) Bool
--traceA = Material False $ \(ray, maxdist) trace _ _ rand -> (maybe False (\(dist,_,_,_) -> dist <= maxdist) $ trace ray, rand)

traceM :: Material Ray (Maybe (Scalar, Intersection, BSDF Colour, Bool))
traceM = Material False $ \ray trace _ _ rand -> runRender (trace ray) rand

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

diffuse :: Material Colour (BSDF Colour)
--diffuse = Material False $ \col _ (Intersection {inorm}) om_i -> holdout { reflected = col } -- Flat
--diffuse = Material False $ \col _ (Intersection {inorm}) om_i -> holdout { reflected = fmap ((/2) . (+1)) inorm } -- Normal
--diffuse = Material False $ \col _ (Intersection {inorm}) om_i -> holdout { reflected = scale (max 0 (om_i `dot` inorm)) col } -- Shaded
diffuse = Material False fun
    where
        fun col _ (Intersection {inorm}) om_i rand = (holdout { reflected = ref }, rand)
            where
                ref = scale (max 0 (om_i `dot` inorm)) col

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

emissive :: Material (Colour, Scalar) (BSDF Colour)
emissive = Material True $ \(col, power) _ _ _ rand -> (holdout { reflected = power `scale` col }, rand)

-- dir `sub` scale (2 * (norm `dot` dir)) norm
mirror :: Material () (BSDF Colour)
--mirror = Material False fun
--    where
--        fun () trace (Intersection {ipos, inorm, iray}) _ rand = (flip (,) rand) . fromMaybe holdout $ do
--            (_, _, bsdf,_) <- trace $ Ray ipos $ (rdir iray) `sub` scale (2 * (inorm `dot` (rdir iray))) inorm
--            return bsdf
mirror = proc () -> do
    (Intersection {ipos, inorm, iray}) <- getIntersection -< ()
    traced <- traceM -< Ray ipos $ (rdir iray) `sub` scale (2 * (inorm `dot` (rdir iray))) inorm
    returnA -< maybe holdout (\(_,_,bsdf,_) -> bsdf) traced

addShader :: Material ((BSDF Colour), (BSDF Colour)) (BSDF Colour)
addShader = arr $ \(s1, s2) -> s1 <> s2
