module GLDisplay.Util where

import HaskRay

import Data.Time.Clock
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Text.Printf

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
