module HaskRay.Out where

import HaskRay.Vector
import HaskRay.Material

import Control.Monad.Identity
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, U, D, Z(..), DIM1, DIM2, DIM3, (:.)(..))
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.IO.BMP
import qualified Data.ByteString.Char8 as B
import Data.List (intersperse)
import Data.List.Split (splitEvery)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Word (Word8)
import System.IO (withFile, IOMode(WriteMode))
import Text.Printf

-- | Simple pixel buffer type.
data PixBuf = PixBuf (Int, Int) ![Colour] deriving (Show, Eq)

-- | Takes a PixBuf, serialises it to PPM format, and saves it to a file.
savePpm :: FilePath -> PixBuf -> IO ()
savePpm dest (PixBuf (w, h) ps) = withFile dest WriteMode $ \handle -> do
    start <- getCurrentTime
    B.hPutStrLn handle $ B.pack fileHead
    let colOut (Vector3 r g b) = B.pack $ (toIntStr r) ++ " " ++ (toIntStr g) ++ " " ++ (toIntStr b)
    let rows = splitEvery w ps
    let rowString ps = B.concat $ intersperse (B.pack " ") $ map colOut ps
    mapM_ (\(r) -> (B.hPutStrLn handle $ rowString r)) rows
    end <- getCurrentTime
    putStrLn $ printf "Render took %s" (show $ diffUTCTime end start)
    where
        fileHead = "P3\n" ++ show w ++ " " ++ show h ++ "\n255"
        toIntStr n = show $ floor $ ((clamp n) ** (1/2.2)) * 255 + 0.5
        clamp n = if n < 0 then 0 else (if n > 1 then 1 else n)

saveBMP :: PixBuf -> FilePath -> IO ()
saveBMP (PixBuf (w, h) cs) path = R.computeP arr >>= (writeImageToBMP path)
    where
        arr :: Array D DIM2 (Word8, Word8, Word8)
        arr = R.backpermute shape flop $ R.map conv $ fromListVector shape cs
        shape = Z :. h :. w
        flop (Z :. i :. j) = Z :. h-i-1 :. j
        conv (Vector3 r g b) = (toEnum $ correctColour r, toEnum $ correctColour g, toEnum $ correctColour b)

saveBMP' :: Array V DIM2 Colour -> FilePath -> IO ()
saveBMP' carr path = R.computeP arr >>= (writeImageToBMP path)
    where
        arr :: Array D DIM2 (Word8, Word8, Word8)
        arr = R.backpermute shape flop $ R.map conv carr
        (Z :. h :. w) = R.extent carr
        shape = R.extent carr
        flop (Z :. i :. j) = Z :. h-i-1 :. j
        conv (Vector3 r g b) = (toEnum $ correctColour r, toEnum $ correctColour g, toEnum $ correctColour b)

toRepa' :: PixBuf -> Array U DIM3 Double
toRepa' (PixBuf (w, h) cs) = runIdentity $ do
    let red = R.map x3 colourArray
    let green = R.map y3 colourArray
    let blue = R.map z3 colourArray
    let carr = R.interleave3 red green blue
    let carr' = R.reshape shape carr
    R.computeP carr'
    where
        flatshape = Z :. w*h
        shape = Z :. w :. h :. (3::Int)
        colourArray = fromListVector flatshape cs

toRepa'' :: PixBuf -> Array U DIM3 Double
toRepa'' (PixBuf (w, h) cs) = runIdentity $ R.computeP $ R.traverse colourArray (const shape') magic
    where
        colourArray :: Array V DIM2 Colour
        colourArray = fromListVector (Z :. w :. h) cs
        shape' :: DIM3
        shape' = Z :. w :. h :. 3
        magic :: (DIM2 -> Colour) -> DIM3 -> Double
        magic lup (Z :. x :. y :. z) = getFromZ z
            where
                getFromZ 0 = x3 $ lup (Z :. x :. y)
                getFromZ 1 = y3 $ lup (Z :. x :. y)
                getFromZ 2 = z3 $ lup (Z :. x :. y)
                getFromZ _ = error "Colour component index out of range"

toRepa :: PixBuf -> Array U DIM3 Double
toRepa (PixBuf (w, h) cs) = (R.fromListUnboxed shape) $ expandColours cs
    where
        shape = Z :. w :. h :. (3::Int)

expandColours :: [Colour] -> [Double]
expandColours [] = []
expandColours (Vector3 r g b:cs) = r : g : b : (expandColours cs)

--saveImage :: (Int, Int) -> [Colour] -> FilePath -> IO ()
--saveImage wh cs file = do
--    arr <- R.copyP $ R.map toWord $ toRepa wh cs
--    runIL $ writeImage file $ RGB arr
--    where
--        toWord n = toEnum $ floor $ ((clamp n) ** (1/2.2)) * 255 + 0.5
--        clamp n = if n < 0 then 0 else (if n > 1 then 1 else n)
