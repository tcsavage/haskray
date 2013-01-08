{-|
This module defines a type class and instances for types that can be rendered in an OpenGL context.
These types include ray tracer 'Object's as well as 'BoundingBox'es and 'Octree's - structures' not normally seen.
These types are included because the OpenGL view is useful for debugging and visualisation.
-}
module GLDisplay.Renderable
(
	Renderable(..)
) where

import HaskRay

import GLDisplay.Util

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Typeable
import Foreign.C.Types

import Graphics.Rendering.OpenGL hiding (Vector3,Sphere,Diffuse,Triangle)
import Graphics.UI.GLUT hiding (Vector3,Vertex,Vertex3(..),Triangle,Sphere,Diffuse,Object)
import qualified Data.Tensor as GLT (Vector3(..), Vertex3(..))

-- | Predicate on types that can be rendered by OpenGL.
class Renderable a where
	-- | Render the object.
	glRender :: a -> IO ()

instance Renderable Object where
	glRender (Object shape material) = do
		setColour material
		--unless (HaskRay.isInfinite shape) $ glRender $ boundingBox shape
		return () `fromMaybe` (liftCast renderSphere shape <|> liftCast renderMesh shape <|> liftCast renderTriangle shape)
			where
				liftCast :: (Typeable a, Typeable b) => (a -> IO ()) -> b -> Maybe (IO ())
				liftCast f s = (liftM f) (cast s)

setColour :: Material -> IO ()
setColour (Diffuse colour) = currentColor $= toGLColor4 colour
setColour _ = currentColor $= toGLColor4 (Vector3 1 1 1)

renderSphere :: Sphere -> IO ()
renderSphere (Sphere center radius) = preservingMatrix $ do
	loadIdentity
	translate $ toGLVec3 center
	renderObject Solid $ Sphere' (CDouble radius) 23 23

renderTriangle :: Triangle -> IO ()
renderTriangle triangle = preservingMatrix $ do
	loadIdentity
	renderPrimitive Triangles $ renderTriangle' triangle

-- | Internal implementation.
renderTriangle' :: Triangle -> IO ()
renderTriangle' (Triangle (Vertex p1 _ _) (Vertex p2 _ _) (Vertex p3 _ _)) = do
		vertex $ toGLVert3 p1
		vertex $ toGLVert3 p2
		vertex $ toGLVert3 p3

renderMesh :: Mesh -> IO ()
renderMesh (Mesh pos tris) = preservingMatrix $ do
	loadIdentity
	translate $ toGLVec3 pos
	mapM_ ((renderPrimitive Triangles) . renderTriangle') tris

instance Renderable BoundingBox where
	glRender (BoundingBox (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) = do
		loadIdentity
		renderPrimitive Lines $ do
			-- 1
			vertex $ GLT.Vertex3 (CDouble x1) (CDouble y1) (CDouble z1)
			vertex $ GLT.Vertex3 (CDouble x2) (CDouble y1) (CDouble z1)
			vertex $ GLT.Vertex3 (CDouble x2) (CDouble y2) (CDouble z1)
			vertex $ GLT.Vertex3 (CDouble x1) (CDouble y2) (CDouble z1)

			-- 2
			vertex $ GLT.Vertex3 (CDouble x2) (CDouble y1) (CDouble z1)
			vertex $ GLT.Vertex3 (CDouble x2) (CDouble y2) (CDouble z1)
			vertex $ GLT.Vertex3 (CDouble x2) (CDouble y2) (CDouble z2)
			vertex $ GLT.Vertex3 (CDouble x2) (CDouble y1) (CDouble z2)

			-- 3
			vertex $ GLT.Vertex3 (CDouble x2) (CDouble y2) (CDouble z2)
			vertex $ GLT.Vertex3 (CDouble x1) (CDouble y2) (CDouble z2)
			vertex $ GLT.Vertex3 (CDouble x1) (CDouble y1) (CDouble z2)
			vertex $ GLT.Vertex3 (CDouble x2) (CDouble y1) (CDouble z2)

drawBox c w = preservingMatrix $ do
	loadIdentity
	translate $ toGLVec3 c
	renderObject Wireframe $ Cube $ CDouble w 

instance Renderable Octree where
	glRender (Leaf c w _) = drawBox c w
	glRender (Branch c w l1 l2 l3 l4 l5 l6 l7 l8) = do
		--currentColor $= (Color4 0 1 1 1)
		--drawBox c w
		let colours = map (\[r,g,b] -> Color4 r g b 1) $ replicateM 3 [1,0.5]
		mapM_ (\(o,c) -> currentColor $= c >> glRender o) $ zip [l1, l2, l3, l4, l5, l6, l7, l8] colours

