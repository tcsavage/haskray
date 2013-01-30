{-# LANGUAGE ExistentialQuantification #-}

module HaskRay.Geometry.Object
(
	-- *** Object Type
	Object(..),

	-- *** Object Utils
	isEmissive,
	intersectOb,
	closestIntersectOb,
	centerOb,
	isInfiniteOb,
	boundingBoxOb,
	mapTextureOb,
	filterSphere,
	expandMeshes,
	boundObjects
) where

import Prelude hiding (isInfinite)

import HaskRay.Vector
import HaskRay.Material
import HaskRay.Geometry.BoundingBox
import HaskRay.Geometry.Classes
import HaskRay.Geometry.Mesh
import HaskRay.Geometry.Ray
import HaskRay.Geometry.Sphere

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Typeable
import GHC.Exts (sortWith)

-- | Heterogeneous container for shapes and an associated material function.
data Object = forall a. Shape a => Object a Material

-- 'Eq' instance for 'Object'. Solution from <http://stackoverflow.com/q/13015949/154598>.
instance Eq Object where
	(==) (Object a _) (Object b _) = Just a == cast b

instance Show Object where
	show (Object a m) = "Object (" ++ (show a) ++ ") (" ++ (show m) ++ ")"

-- | Find out of the object has an emissive material.
isEmissive :: Object -> Bool
isEmissive (Object _ mat) = case mat of
	(Emissive _ _) -> True
	otherwise -> False

-- | Find intersections for an object's 'Shape'.
intersectOb :: Ray -> Object -> Maybe (Scalar, Intersection)
intersectOb r (Object s mat) = intersect r (s, mat)

closestIntersectOb :: Ray -> [Object] -> Maybe (Scalar, Intersection, Object)
closestIntersectOb r objs
	| null obis = Nothing
	| otherwise = Just $ head obis
	where
		injectOb ob Nothing = Nothing
		injectOb ob (Just (d, i)) = Just (d, i, ob)
		obis = sortWith (\(t, _, _) -> t) $ catMaybes $ map (\o -> injectOb o $ intersectOb r o) objs

centerOb :: Object -> Vec3
centerOb (Object s _) = center s

isInfiniteOb :: Object -> Bool
isInfiniteOb (Object s _) = isInfinite s

boundingBoxOb :: Object -> BoundingBox
boundingBoxOb (Object s _) = boundingBox s

mapTextureOb :: Object -> Vec3 -> Vec2
mapTextureOb (Object s _) = mapTexture s

-- | Get a lost of spheres from a list of objects.
filterSphere :: [Object] -> [Sphere]
filterSphere objs = catMaybes $ map (\(Object s _) -> cast s) objs

-- | Expand Meshes into triangles and merge back into object list.
expandMeshes :: [Object] -> [Object]
expandMeshes objs = objs >>= (\x@(Object a m) -> maybe [x] (\y -> map (flip Object $ m) (expandMesh y)) (cast a))

expandMesh :: Mesh -> [Triangle]
expandMesh (Mesh pos tris) = map (translateTriangle pos) tris
--expandMesh (Mesh _ tris) = tris

-- | Get the bounding box of several objects.
boundObjects :: [Object] -> BoundingBox
boundObjects os = BoundingBox ev1 ev2
	where
		bbs = join $ map (boundingPoints . boundingBoxOb) $ filter (not . isInfiniteOb) os
		boundingPoints (BoundingBox v1 v2) = [v1, v2]
		(ev1, ev2) = minmaxPoints bbs
