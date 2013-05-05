{-# LANGUAGE NamedFieldPuns, KindSignatures, FlexibleInstances, OverlappingInstances #-}

module HaskRay.Material
(
Colour,
multColour,
floatingToByte,
gammaCorrect,
Material(..),
evalMaterial,
BSDF(..),
Texture,
loadTexture,
indexTextureUV,
diffuse,
emissive,
mirror,
holdout
) where

import HaskRay.Vector
import HaskRay.Ray

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

data Material a (b :: *) = Material {
    isEmissive :: !Bool,
    closure :: !(a -> (Ray -> Maybe (Scalar, Intersection, BSDF Colour)) -> Intersection -> Vec3 -> b)
}

evalMaterial :: Material () (BSDF a) -> (Ray -> Maybe (Scalar, Intersection, BSDF Colour)) -> Intersection -> Vec3 -> BSDF a
evalMaterial mat = closure mat ()

instance Category Material where
    id = Material False $ \inp _ _ _ -> inp
    Material im1 cl1 . Material im2 cl2 = Material (im1 || im2) $ \inp trace int om_i -> cl1 (cl2 inp trace int om_i) trace int om_i

instance Arrow Material where
    arr f = Material False $ \inp _ _ _ -> f inp
    first (Material im cl) = Material im $ \(x1, x2) trace int om_i -> (cl x1 trace int om_i, x2)

getIntersection :: Material () Intersection
getIntersection = Material False $ \() _ int _ -> int

getInidentRay :: Material () Vec3
getInidentRay = Material False $ \() _ _ om_i -> om_i

traceA :: Material (Ray, Scalar) Bool
traceA = Material False $ \(ray, maxdist) trace _ _ -> maybe False (\(dist,_,_) -> dist <= maxdist) $ trace ray

diffuse :: Material Colour (BSDF Colour)
diffuse = Material False $ \col _ (Intersection {inorm}) om_i -> holdout { reflected = col }
--diffuse = Material False $ \col _ (Intersection {inorm}) om_i -> holdout { reflected = fmap ((/2) . (+1)) inorm } -- Normal
--diffuse = Material False $ \col _ (Intersection {inorm}) om_i -> holdout { reflected = scale (max 0 (om_i `dot` inorm)) col }

emissive :: Material (Colour, Scalar) (BSDF Colour)
emissive = Material True $ \(col, power) _ _ _ -> holdout { reflected = power `scale` col }

-- dir `sub` scale (2 * (norm `dot` dir)) norm
mirror :: Material () (BSDF Colour)
mirror = Material False $ \() trace (Intersection {ipos, inorm, iray}) _ -> fromMaybe holdout $ do
    (_, _, bsdf) <- trace $ Ray ipos $ (rdir iray) `sub` scale (2 * (inorm `dot` (rdir iray))) inorm
    return bsdf

addShader :: Material ((BSDF Colour), (BSDF Colour)) (BSDF Colour)
addShader = arr $ \(s1, s2) -> s1 <> s2
