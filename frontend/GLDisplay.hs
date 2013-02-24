module GLDisplay where

import HaskRay hiding (Sphere, lookAt)
import qualified HaskRay as HR

import GLDisplay.Display
import GLDisplay.Input
import GLDisplay.Util
import qualified  GLDisplay.UI as UI
import GLDisplay.CameraMode

import Control.Concurrent.Async
import Data.Array.Repa (Array, Z(..), DIM2, (:.)(..), extent)
import Data.Array.Repa.Repr.Vector (V)
import Data.IORef
import Text.Printf

import Graphics.UI.GLUT hiding (Vector3,Vertex,Vertex3(..),Triangle)

glDisp :: Settings -> Scene -> Array V DIM2 Colour -> IO ()
glDisp settings scene array = do
	let (Z :. h :. w) = extent array
	-- Make IORef tracking display mode
	viewModeIOR <- newIORef RenderDisplay
	cameraModeIOR <- newIORef (CameraMode True 0 0)
	UI.withAwesomium (w, h) [UI.handleActions viewModeIOR] $ \wv -> do
		-- Start GLUT and OpenGL
		initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
		initialWindowSize $= Size ((fromIntegral w) :: GLsizei) ((fromIntegral h) :: GLsizei)
		(progname, args) <- getArgsAndInitialize
		wnd <- createWindow $ printf "HaskRay Render (%dx%d)" w h

		-- Make image array (async)
		imgArray <- async $ makeForeignPtr array

		-- Setup callbacks
		displayCallback $= display wv scene viewModeIOR cameraModeIOR (w, h) imgArray
		keyboardMouseCallback $= Just (keyboardMouse wv viewModeIOR cameraModeIOR settings scene imgArray)
		motionCallback $= Just (UI.mouseMove wv)
		passiveMotionCallback $= Just (UI.mouseMove wv)
		reshapeCallback $= Just (UI.reshape wv)

		-- Set up alpha blending (for Awesomium over OpenGL)
		blend $= Enabled
		blendFunc $= (One, OneMinusSrcAlpha)
		depthFunc $= Just Less

		-- Start rendering
		mainLoop
