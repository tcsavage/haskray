module GLDisplay.Input where

import HaskRay

import GLDisplay.Display
import GLDisplay.Util
import qualified  GLDisplay.UI as UI
import GLDisplay.CameraMode

import Graphics.UI.Awesomium

import Control.Concurrent.Async
import Data.IORef
import Foreign.C.Types
import Foreign.Ptr

import Graphics.UI.GLUT hiding (Vector3,Vertex,Vertex3(..),Triangle)

keyboardMouse :: WebViewHandle -> IORef ViewMode -> IORef CameraMode -> Settings -> Scene -> Async (Ptr CChar) -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse _ _ _ settings scene apixels (MouseButton LeftButton) Down (Modifiers Down Up Up) (Position x y) = putStrLn $ examineTreeAt settings scene (fromIntegral x, fromIntegral y)
keyboardMouse _ viewModeIOR _ _ _ _ (Char ' ') Down _ _ = modifyIORef viewModeIOR (\x -> if x == RenderDisplay then GLView else RenderDisplay) >> readIORef viewModeIOR >>= print
keyboardMouse _ _ cameraModeIOR _ _ _ (SpecialKey KeyLeft) Down (Modifiers Up Up Down) _ = do
	(CameraMode m x y) <- readIORef cameraModeIOR
	writeIORef cameraModeIOR (CameraMode m (x+2) y)
keyboardMouse _ _ cameraModeIOR _ _ _ (SpecialKey KeyRight) Down (Modifiers Up Up Down) _ = do
	(CameraMode m x y) <- readIORef cameraModeIOR
	writeIORef cameraModeIOR (CameraMode m (x-2) y)
keyboardMouse wv _ _ _ _ _ k ks m p = UI.keyboardMouse wv k ks m p
