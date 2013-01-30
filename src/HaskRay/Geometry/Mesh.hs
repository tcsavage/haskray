{-# LANGUAGE DeriveDataTypeable #-}

module HaskRay.Geometry.Mesh
(
	-- *** Vertex
	Vertex(..),
	translateVertex,

	-- *** Triangle
	Triangle(..),
	translateTriangle,
	pointOnTriangle,

	-- *** Mesh
	Mesh(..)
) where

import HaskRay.Vector
import HaskRay.Material
import HaskRay.Geometry.BoundingBox
import HaskRay.Geometry.Classes
import HaskRay.Geometry.Plane
import HaskRay.Geometry.Ray

import Control.Applicative
import Control.Monad
import Data.Typeable

-- | A vertex has a position, normal and UV coordinates.
data Vertex = Vertex Vec3 Vec3 Vec2 deriving (Show, Read, Eq)

-- | Moves a vertex by a vector
translateVertex :: Vec3 -> Vertex -> Vertex
translateVertex vec (Vertex p n uv) = Vertex (p `add` vec) n uv

-- | Triangles are composed of three vertices.
data Triangle = Triangle Vertex Vertex Vertex deriving (Show, Read, Eq, Typeable)

-- | Moves a triangle by a vector.
translateTriangle :: Vec3 -> Triangle -> Triangle
translateTriangle vec (Triangle v1 v2 v3) = Triangle (tf v1) (tf v2) (tf v3)
	where
		tf = translateVertex vec

-- | Given a UV coordinate, get its position on the triangle in 3D space.
pointOnTriangle :: Vec2 -> Triangle -> Vec3
pointOnTriangle (Vector2 u v) (Triangle (Vertex p1 _ _) (Vertex p2 _ _) (Vertex p3 _ _)) = ((1-u-v) `scale` p1) `add` (u `scale` p2) `add` (v `scale` p3)

-- | Project a 3D world-space coordinate into a 2D triangle-space coordinate.
pointToUV :: Vec3 -> Triangle -> Vec2
pointToUV p (Triangle (Vertex a _ _) (Vertex b _ _) (Vertex c _ _)) = Vector2 u v
	where
		v0 = c `sub` a
		v1 = b `sub` a
		v2 = p `sub` a
		dot00 = v0 `dot` v0
		dot01 = v0 `dot` v1
		dot02 = v0 `dot` v2
		dot11 = v1 `dot` v1
		dot12 = v1 `dot` v2
		invDenom = 1 / (dot00 * dot11 - dot01 * dot01)
		u = (dot11 * dot02 - dot01 * dot12) * invDenom
		v = (dot00 * dot12 - dot01 * dot02) * invDenom

-- | Transform a triagnle-space coordinate into texture-space.
textureSpace :: Vec2 -> Triangle -> Vec2
textureSpace coord@(Vector2 u v) (Triangle (Vertex _ _ a@(Vector2 ua va)) (Vertex _ _ b@(Vector2 _ vb)) (Vertex _ _ c@(Vector2 uc _))) = a `add` (scale v ab) `add` (scale u ac)
	where
		adj = Vector2 (uc-ua) (vb-va)
		ab = b `sub` a
		ac = c `sub` a

instance Shape Triangle where
	--intersect ray (Triangle (Vertex p1 _ _) (Vertex p2 _ _) (Vertex p3 _ _), m) = do
	--	is@(_, (Intersection _ p _ _)) <- intersect ray (plane, m)
	--	if pointInTriangle p p1 p2 p3 then return is else Nothing
	--	where
	--		plane = Plane norm dist
	--			where
	--				norm = normalize ((p2 `sub` p1) `cross` (p3 `sub` p1))
	--				dist = norm `dot` p1
	--		testSideage p1 p2 a b = (cp1 `dot` cp2) >= 0
	--			where
	--				cp1 = (b `sub` a) `cross` (p1 `sub` a)
	--				cp2 = (b `sub` a) `cross` (p2 `sub` a)
	--		pointInTriangle p a b c = (testSideage p a b c) && (testSideage p b a c) && (testSideage p c a b)

	-- Barycentric technique. From <http://www.blackpawn.com/texts/pointinpoly/default.html>
	intersect ray (tri@(Triangle (Vertex p1 _ _ ) (Vertex p2 _ _ ) (Vertex p3 _ _ )), m) = do
		is@(_, (Intersection _ p _ _)) <- intersect ray (plane, m)
		if pointInTriangle p then Just is else Nothing
		where
			plane = Plane norm dist
				where
					norm = normalize ((p2 `sub` p1) `cross` (p3 `sub` p1))
					dist = negate $ norm `dot` p1
			pointInTriangle p = (u >= 0) && (v >= 0) && (u + v < 1)
				where
					(Vector2 u v) = pointToUV p tri

	center (Triangle (Vertex v1 _ _) (Vertex v2 _ _) (Vertex v3 _ _)) = scale (1/3) (v1 `add` v2 `add` v3)
	boundingBox (Triangle (Vertex v1 _ _) (Vertex v2 _ _) (Vertex v3 _ _)) = BoundingBox maxp minp
		where
			(minp, maxp) = minmaxPoints [v1,v2,v3]
	--mapTexture tri point = pointToUV point tri
	mapTexture tri point = textureSpace (pointToUV point tri) tri

-- | A mesh is a list of triangles.
data Mesh = Mesh Vec3 [Triangle] deriving (Show, Read, Eq, Typeable)

instance Shape Mesh where
	intersect ray (mesh@(Mesh origin tris), m) = if intersectBox ray (boundingBox mesh) then test else Nothing
		where
			test = msum $ map (intersect ray) $ zip (map (translateTriangle origin) tris) $ repeat m
	center (Mesh origin tris) = (scale (fromIntegral $ length tris)) . (foldr add (Vector3 0 0 0)) . (map center) $ tris
	boundingBox (Mesh pos ts) = BoundingBox maxp minp
		where
			tris = map (translateTriangle pos) ts
			verts = join $ map (\(Triangle (Vertex v1 _ _) (Vertex v2 _ _) (Vertex v3 _ _)) -> [v1,v2,v3]) tris
			(minp, maxp) = minmaxPoints verts
	mapTexture = error "Can't texture mesh (yet)"
