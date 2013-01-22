module HaskRay.Texture where

import HaskRay.Vector
import HaskRay.Material

import Control.Monad.Identity
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, U, D, Z(..), DIM1, DIM2, DIM3, (:.)(..))
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.IO.BMP
import Data.Word

width = 50
height = 50

--texture :: [[Colour]]
--texture = [Vector3 r 0 b| r <- [0,(1/width)..1], b <- [0,(1/height)..1]]

saveBMP :: Array V DIM2 Colour -> FilePath -> IO ()
saveBMP inp path = R.computeP arr >>= (writeImageToBMP path)
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
		u' = u*w
		v' = v*h
