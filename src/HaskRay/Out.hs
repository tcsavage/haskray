{-# LANGUAGE ScopedTypeVariables #-}

module HaskRay.Out
(
saveBMP,
makeForeignPtr
) where

import HaskRay.Vector
import HaskRay.Material

import Control.Exception (catch, IOException)
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, D, Z(..), DIM2, DIM3, (:.)(..))
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.IO.BMP
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.ForeignPtr.Safe

-- | Save an array of 'Colour' to BMP file. Uses repa-io.
saveBMP :: Array V DIM2 Colour -> FilePath -> IO ()
saveBMP carr path = catch (R.computeP arr >>= writeImageToBMP path) $ \err -> do
    putStrLn "HaskRay.Out.saveBMP: Failed to save BMP file:"
    print (err :: IOException)
    where
        arr :: Array D DIM2 (Word8, Word8, Word8)
        arr = R.backpermute shape flop $ R.map (toTriple . fmap (floatingToByte . gammaCorrect)) carr
        (Z :. h :. _) = R.extent carr
        shape = R.extent carr
        flop (Z :. i :. j) = Z :. h-i-1 :. j
        toTriple (Vector3 r g b) = (r, g, b)

{-|
Write a 'Colour' array into an area of raw memory.

The intermediate array has 4 elements in the inner array for RGBA.
-}
makeForeignPtr :: Array V DIM2 Colour -> IO (ForeignPtr CChar)
makeForeignPtr arr = do
    arr' :: Array F DIM3 CChar <- R.computeP $ R.traverse arr (\(Z :. h :. w) -> Z :. h :. w :. 4) magic
    return $ toForeignPtr arr'
    where
        magic lookup (Z :. h :. w :. c) = case c of
            0 -> toByte $ x3 $ lookup (Z :. h :. w)
            1 -> toByte $ y3 $ lookup (Z :. h :. w)
            2 -> toByte $ z3 $ lookup (Z :. h :. w)
            3 -> 1 -- Alpha defaults to 1.
            _ -> error "HaskRay.Out.makeForeignPtr: Component dimension > 2"
        toByte = CChar . toInt
        toInt n = fromIntegral . floor $ (clamp n ** (1/2.2)) * 255 + 0.5
        clamp n
            | n < 0 = 0
            | n > 1 = 1
            | otherwise = n
