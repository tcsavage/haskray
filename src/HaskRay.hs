module HaskRay
(
-- * Vectors
module HaskRay.Vector,
-- * Material
Colour(..),
Material(..),
-- * Geometry
module HaskRay.Geometry,
-- * Octree
module HaskRay.Octree,
-- * Ray Tree
module HaskRay.RayTree,
-- * Projection
View(..),
-- * Settings
module HaskRay.Settings,
-- * Scene
module HaskRay.Scene,
-- * Image Ouput
PixBuf(..),
savePpm,
-- * High-level Operations
render,
getPixelForest,
examineTreeAt,
-- * Parser
module HaskRay.Parser
) where

import System.IO
import System.Random (mkStdGen, StdGen)
import Control.Monad.Random
import qualified Debug.Trace as DT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Parallel.Strategies (using, parList, rseq)

import HaskRay.Vector
import HaskRay.Material
import HaskRay.Geometry
import HaskRay.Octree
import HaskRay.Projection
import HaskRay.Settings
import HaskRay.RayTree
import HaskRay.RayTree.String
import HaskRay.Parser
import HaskRay.Scene

-- | Simple pixel buffer type.
data PixBuf = PixBuf (Int, Int) ![Colour] deriving (Show, Eq)

-- | Takes a PixBuf, serialises it to PPM format, and saves it to a file.
savePpm :: FilePath -> PixBuf -> IO ()
savePpm dest (PixBuf (w, h) ps) = withFile dest WriteMode $ \handle -> do
    B.hPutStrLn handle $ B.pack $ "P3\n" ++ show w ++ " " ++ show h ++ "\n255"
    let colOut (Vector3 r g b) = B.pack $ (toIntStr r) ++ " " ++ (toIntStr g) ++ " " ++ (toIntStr b)
    let rows = splitEvery w ps
    let rowString ps = B.concat $ intersperse (B.pack " ") $ map colOut ps
    sequence_ (map (\(r,n) -> (B.hPutStrLn handle $ rowString r) >> print n) (zip rows [1..]) `using` parList rseq) -- Use parallel eval strategy
    --B.hPutStrLn handle $ B.unlines $ (map rowString rows `using` parList rseq) -- Use parallel eval strategy
    --sequence_ (map ((B.hPutStrLn handle) . rowString) rows `using` parList rseq)
    where
        --toIntStr n = show $ floor $ ((clamp n) ** (1/2.2)) * 255 + 0.5
        toIntStr n = show $ floor $ ((clamp n) ** (1/2.2)) * 255 + 0.5
        clamp n = if n < 0 then 0 else (if n > 1 then 1 else n)

-- | Render a scene with given settings.
render :: Settings -> Scene -> PixBuf
render settings@(Settings w h _ rand) (Scene os view) = PixBuf (w, h) pixels
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view -- [[Ray]]
        pixels = (flip evalRand) rand $ sequence (map (\x -> tracePixel obs x >>= (return . evalPixel)) sampleRays `using` parList rseq)

getPixelForest :: Settings -> Scene -> [Pixel]
getPixelForest settings@(Settings _ _ _ rand) (Scene os view) = forest
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view
        forest = (flip evalRand) rand $ mapM (tracePixel obs) sampleRays

-- | Return a textual representation of the ray tree for a given pixel (instead of rendering).
examineTreeAt :: Settings -> Scene -> (Int, Int) -> String
examineTreeAt settings@(Settings w _ _ rand) (Scene os view) (x, y) = treeString tree
    where
        obs = mkObStruct os
        sampleRays = makeCameraRays settings view
        tree = (flip evalRand) rand $ tracePixel obs (sampleRays !! (y*w + x))
