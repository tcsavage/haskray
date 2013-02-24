module GLDisplay.Display where

import HaskRay hiding (Sphere, lookAt)
import qualified HaskRay as HR

import GLDisplay.Renderable
import GLDisplay.Util
import GLDisplay.CameraMode

import Graphics.UI.Awesomium
import qualified Graphics.UI.Awesomium.OpenGL as AGL

import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import Data.Typeable
import Foreign.C.Types
import Foreign.ForeignPtr.Safe
import Foreign.Ptr
import System.Exit
import Text.Printf

import Graphics.Rendering.OpenGL hiding (Vector3)
import Graphics.UI.GLUT hiding (Vector3,Vertex,Vertex3(..),Triangle,Sphere)
import qualified Data.Tensor as GLT (Vector3(..), Vertex3(..))

display :: WebViewHandle -> Scene -> IORef ViewMode -> IORef CameraMode -> (Int, Int) -> Async (ForeignPtr CChar) -> IO ()
display wv scene viewModeIOR cameraModeIOR wh apixels = do
    viewMode <- readIORef viewModeIOR
    case viewMode of
        RenderDisplay -> displayRender wh apixels
        GLView -> displayGL cameraModeIOR scene wh apixels
    update
    depthFunc $= Just Always
    AGL.display wv
    depthFunc $= Just Less
    swapBuffers
    postRedisplay Nothing

displayRender :: (Int, Int) -> Async (ForeignPtr CChar) -> IO ()
displayRender (w, h) apixels = do
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
    matrixMode $= (Modelview 0)
    loadIdentity
    status <- poll apixels
    maybe waiting finished status
    where
        waiting = do
            currentRasterPosition $= Vertex4 50.0 50.0 0.0 1.0
            let message = printf "Rendering (%d, %d), please wait..." w h
            renderString Fixed8By13 message
        finished (Prelude.Left err) = print err
        finished (Prelude.Right pixels) = do
            currentRasterPosition $= Vertex4 0.0 (fromIntegral h) 0.0 1.0
            pixelZoom $= (1.0, -1.0) -- OpenGL's coordinate system starts in bottom left. We start top left, so the image must be fliped.
            withForeignPtr pixels $ \p -> drawPixels (Size (fromIntegral w :: GLsizei) (fromIntegral h :: GLsizei)) (PixelData RGBA UnsignedByte p)

displayGL :: IORef CameraMode -> Scene -> (Int, Int) -> Async (ForeignPtr CChar) -> IO ()
displayGL cameraModeIOR (Scene os (View p1 p2 uvec fov)) (w,h) _ = do
    (CameraMode useScene xRot yRot) <- readIORef cameraModeIOR
    let aspectRatio = (fromIntegral w)/(fromIntegral h)
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Projection
    loadIdentity
    perspective (CDouble fov) aspectRatio 0.01 1000
    lookAt (toGLVert3 p1) (toGLVert3 p2) (toGLVec3 uvec)
    rotate (CDouble xRot) (toGLVec3 $ Vector3 0 1 0)
    matrixMode $= (Modelview 0)
    loadIdentity
    mapM_ glRender os
    loadIdentity
    glRender $ mkOctree os
