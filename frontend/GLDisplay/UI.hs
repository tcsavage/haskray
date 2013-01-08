module GLDisplay.UI where

import GLDisplay.Util
import Paths_HaskRay (getDataFileName, getDataDir)

import Graphics.UI.Awesomium
import qualified Graphics.UI.Awesomium.OpenGL as AGL

import Control.Monad
import Control.Monad.Instances
import Data.IORef
import Foreign.Ptr
import Text.Printf

import Graphics.UI.GLUT hiding (Vector3,Vertex,Triangle,color)

withAwesomium :: (Int, Int) -> [WebViewHandle -> String -> [JsValueHandle] -> IO ()] -> (WebViewHandle -> IO ()) -> IO ()
withAwesomium (w,h) handlers fun = do
	-- Start Awesomium
	putStrLn "Starting Awesomium"
	css <- getDataFileName "ui/Awesomium.css" >>= readFile
	Graphics.UI.Awesomium.initialize (defaultConfig { customCss = css, logLevel = Verbose })
	putStrLn "Creating WebView"
	wv <- newWebView w h False
	putStrLn "Loading web page"
	getDataDir >>= setBaseDirectory
	loadFile wv "ui/UI.html" ""
	waitOn wv
	setTransparent wv True
	gainFocus wv

	-- Set up JS API with custom actions
	actionsRef <- newIORef $ sequence handlers wv
	jscb <- mkCallback (callbackHandler actionsRef)
	registerApiHandler wv jscb
	makeApi wv "hs" ["reload", "switchmode", "test", "quit", "foo"] -- "hs.test()"
	putStrLn "Done"

	fun wv

	-- Clean up
	shutdown
	Foreign.Ptr.freeHaskellFunPtr jscb

-- Update until webview is ready
waitOn :: WebViewHandle -> IO ()
waitOn wv = do
	loading <- isLoading wv
	if loading then update >> waitOn wv else return ()

mouseMove :: WebViewHandle -> Position -> IO ()
mouseMove wv position@(Position x y) = do
	injectMouse wv (fromIntegral x) (fromIntegral y)

keyboardMouse :: WebViewHandle -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse wv (MouseButton btn) Down _ _ = injectMouseDown wv (glutToAwesomium btn)
keyboardMouse wv (MouseButton btn) Up _ _ = injectMouseUp wv (glutToAwesomium btn)
keyboardMouse _ _ _ _ _ = return ()

glutToAwesomium :: Graphics.UI.GLUT.MouseButton -> Graphics.UI.Awesomium.MouseButton
glutToAwesomium LeftButton = Graphics.UI.Awesomium.Left
glutToAwesomium RightButton = Graphics.UI.Awesomium.Right
glutToAwesomium MiddleButton = Graphics.UI.Awesomium.Middle

reshape :: WebViewHandle -> Size -> IO ()
reshape wv s@(Size w h) = do
	viewport $= ((Position 0 0), s)
	resize wv (fromIntegral w) (fromIntegral h) True 0
	postRedisplay Nothing

handleActions :: IORef ViewMode -> WebViewHandle -> String -> [JsValueHandle] -> IO ()
handleActions _ wv "reload" _ = reload wv
handleActions vmr wv "switchmode" _ = modifyIORef vmr (\x -> if x == RenderDisplay then GLView else RenderDisplay)
handleActions _ _ "test" args = do
	as <- mapM showJsValue args
	putStrLn $ printf "Testing 1.2.3: %s" $ show as
handleActions _ _ "quit" _ = leaveMainLoop
handleActions _ _ str args = putStrLn $ printf "Unassigned API action for '%s'" str
