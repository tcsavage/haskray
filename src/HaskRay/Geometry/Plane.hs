{-# LANGUAGE DeriveDataTypeable #-}

module HaskRay.Geometry.Plane
(
	Plane(..)
) where

import HaskRay.Vector
import HaskRay.Material
import HaskRay.Geometry.Classes
import HaskRay.Geometry.Ray
import HaskRay.Geometry.Util

import Data.Typeable

{-|
Defines an infinite plane.

> Plane (Vector3 a b c) d => ax + by + cz + d = 0
-}
data Plane = Plane Vec3 Scalar deriving (Show, Read, Eq, Typeable)

instance Shape Plane where
	intersect ray@(Ray origin dir) ((Plane normal d), material)
		| vd == 0 = Nothing
		| t > epsilon = Just (t, Intersection (if (vd > 0) then (neg normal) else normal) hitpoint ray material)
		| otherwise = Nothing
		where
			vd = (normalize normal) `dot` dir
			v0 = negate (((normalize normal) `dot` origin) + d)
			t = v0 / vd
			hitpoint = positionAtTime ray t
	center (Plane normal d) = scale d normal
	isInfinite = const True
	boundingBox = error "Infinite plane"
