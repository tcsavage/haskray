module GLDisplay.Util where

import HaskRay

import Control.Parallel.Strategies (using, parList, rseq)
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr

import Graphics.Rendering.OpenGL hiding (Vector3)
import qualified Data.Tensor as GLT (Vector3(..), Vertex3(..))

data ViewMode = RenderDisplay | GLView deriving (Show, Eq)

toGLVec3 :: Vector3 Double -> GLT.Vector3 GLdouble
toGLVec3 (Vector3 x y z) = GLT.Vector3 (CDouble x) (CDouble y) (CDouble z)

toGLVert3 :: Vector3 Double -> GLT.Vertex3 GLdouble
toGLVert3 (Vector3 x y z) = GLT.Vertex3 (CDouble x) (CDouble y) (CDouble z)

toGLColor4 :: Colour -> Color4 GLfloat
toGLColor4 (Vector3 r g b) = Color4 (CFloat $ dtof r) (CFloat $ dtof g) (CFloat $ dtof b) 1
    where
        dtof :: Double -> Float
        dtof n = (uncurry encodeFloat) (decodeFloat n)

colourToBytes :: Colour -> [CChar]
colourToBytes (Vector3 r g b) = [toByte r, toByte g, toByte b, toByte 1]
    where
        toByte = CChar . toInt
        toInt n = fromIntegral . floor $ (clamp n ** (1/2.2)) * 255 + 0.5
        clamp n
            | n < 0 = 0
            | n > 1 = 1
            | otherwise = n

coloursToBytes :: [Colour] -> [CChar]
coloursToBytes cs = concat (map colourToBytes cs `using` parList rseq)

makeColourArray :: [Colour] -> IO (Ptr CChar)
makeColourArray = newArray . coloursToBytes
