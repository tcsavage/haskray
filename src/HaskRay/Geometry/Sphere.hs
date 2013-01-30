{-# LANGUAGE DeriveDataTypeable #-}

module HaskRay.Geometry.Sphere
(
	Sphere(..)
) where

import HaskRay.Vector
import HaskRay.Material
import HaskRay.Geometry.BoundingBox
import HaskRay.Geometry.Classes
import HaskRay.Geometry.Ray
import HaskRay.Geometry.Util

import Control.Applicative
import Data.Maybe
import Data.Typeable
import GHC.Exts (sortWith)

{-|
Defines a mathematical sphere.
-}
data Sphere = Sphere Vec3 Scalar deriving (Show, Read, Eq, Typeable)

instance Shape Sphere where
	intersect ray@(Ray origin dir) ((Sphere center rad), material) = listToMaybe is
		where
			a = magSquared dir
			b = 2 * ( dir `dot` (origin `sub` center))
			c = (magSquared (origin `sub` center)) - rad^2
			times = filter (> epsilon) (roots a b c)
			normal_at_time t = normalize ((positionAtTime ray t) `sub` center)
			intersection_at_time t = Intersection (normal_at_time t) (positionAtTime ray t) ray material
			is = sortWith (\(t, _) -> t) $ map (\t -> (t,intersection_at_time t)) times
	center (Sphere c _) = c
	boundingBox (Sphere c r) = BoundingBox (c `add` pure r) (c `sub` pure r)
	mapTexture (Sphere center rad) p = Vector2 u v
		where
			d = normalize (center `sub` p)
			u = 0.5 + (atan2 (z3 d) (x3 d) / (2*pi))
			v = 0.5 - 2 * (asin (y3 d) / (2*pi))
