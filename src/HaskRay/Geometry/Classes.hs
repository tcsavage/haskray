module HaskRay.Geometry.Classes
(
	-- *** Shape Type Class
	Shape(..)
) where

import HaskRay.Vector
import HaskRay.Material
import HaskRay.Geometry.BoundingBox
import HaskRay.Geometry.Ray

import Data.Typeable

-- | Predicate on types that support 'intersect'.
class (Eq s, Show s, Typeable s) => Shape s where
	-- | Find a list of intersections (and distances) between a ray and a shape.
	intersect :: Ray -> (s, Material) -> Maybe (Scalar, Intersection)
	-- | Gets the center of the shape.
	center :: s -> Vec3
	-- | Should return true if the shape is infinite. Defaults to False.
	isInfinite :: s -> Bool
	isInfinite = const False
	-- | Calculates the axially-aligned bounding box of the shape
	boundingBox :: s -> BoundingBox
