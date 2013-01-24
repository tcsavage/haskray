module HaskRay.Material where

import HaskRay.Vector

import Control.Monad.Identity
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, U, D, Z(..), DIM1, DIM2, DIM3, (:.)(..))
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.IO.BMP
import Data.Word

-- | Type alias for RGB colours.
type Colour = Vec3

averageColour :: [Colour] -> Colour
averageColour cs = scale (1/(fromIntegral $ length cs)) $ addColours cs
	where
		addColours = foldr add (Vector3 0 0 0)

multColour :: Colour -> Colour -> Colour
multColour (Vector3 r1 g1 b1) (Vector3 r2 g2 b2) = Vector3 (r1 * r2) (g1 * g2) (b1 * b2)

addColour :: Colour -> Colour -> Colour
addColour (Vector3 r1 g1 b1) (Vector3 r2 g2 b2) = Vector3 (r1 + r2) (g1 + g2) (b1 + b2)

clampColour :: Colour -> Colour
clampColour = fmap (\x -> if x < 0 then 0 else (if x > 1 then 1 else x))

correctColour :: Double -> Int
correctColour n = floor $ ((clamp n) ** (1/2.2)) * 255 + 0.5
    where
        clamp n = if n < 0 then 0 else (if n > 1 then 1 else n)

-- | Defines surface properties.
data Material = Diffuse Colour | Emissive Colour Scalar | Reflective | Transmissive Double Double | Texture Texture deriving (Show, Read, Eq)

type Texture = Array V DIM2 Colour

width, height :: Num a => a
width = 50
height = 50

--texture :: [[Colour]]
--texture = [Vector3 r 0 b| r <- [0,(1/width)..1], b <- [0,(1/height)..1]]

saveBMPTex :: Array V DIM2 Colour -> FilePath -> IO ()
saveBMPTex inp path = R.computeP arr >>= (writeImageToBMP path)
    where
        arr :: Array D DIM2 (Word8, Word8, Word8)
        arr = R.map conv inp
        conv (Vector3 r g b) = (toEnum $ correctColour r, toEnum $ correctColour g, toEnum $ correctColour b)

empty :: Array U DIM2 Int
empty = R.fromListUnboxed (Z :. width :. height) [1..width*height]

texture :: Array V DIM2 Colour
texture = runIdentity $ R.computeP $ R.traverse empty (const $ R.extent empty) magic
    where
        magic :: (DIM2 -> Int) -> DIM2 -> Colour
        magic _ (Z :. x :. y) = Vector3 ((fromIntegral x)/(fromIntegral width)) ((((fromIntegral x)/(fromIntegral width)) + ((fromIntegral y)/(fromIntegral height))) / 4) ((fromIntegral y)/(fromIntegral height))

texUV :: Array V DIM2 Colour -> Vec2 -> Colour
texUV tex (Vector2 u v)
    | u > 1 || v > 1 || u < 0 || v < 0 = error "UV coords out of range"
    | otherwise = R.index tex (Z :. u' :. v')
    where
        (Z :. w :. h) = R.extent tex
        u' = floor $ u*(fromIntegral w)
        v' = floor $ v*(fromIntegral h)
