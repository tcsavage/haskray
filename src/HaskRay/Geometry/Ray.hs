module HaskRay.Geometry.Ray
(
	-- *** Ray Type
	Ray(..),

	-- *** Intersection
	Intersection(..),
	epsilon,
	closestIntersection
) where

import HaskRay.Vector
import HaskRay.Material

import GHC.Exts (sortWith)

-- | Semi-infinate ray.
data Ray = Ray Vec3 Vec3 -- ^ Arguments: @'Ray' (origin :: 'Vec3') (direction :: 'Vec3')@.
	deriving (Show, Read, Eq)

-- | Records an intersection with geometry.
data Intersection = Intersection Vec3 Vec3 Ray Material -- ^ Arguments: @'Intersection' (normal :: 'Vec3') (pontOfIntersection :: 'Vec3') (ray :: 'Ray') (material :: 'Material')@
	deriving (Show, Read, Eq)

{-
Minimum distance for ray intersection to be considered important.
This is due to limited numerical precision with floating point numbers. A ray could be reflected and then be immediately registered as intersecting with the object it just bounced off.
-}
epsilon :: Scalar
epsilon = 0.001

-- | Grabs the intersection with the shortest distance.
closestIntersection :: [(Scalar, Intersection)] -> Intersection
closestIntersection is = snd . head $ sortWith (\(t, _) -> t) is
