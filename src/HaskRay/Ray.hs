module HaskRay.Ray
(
-- *** Ray Type
Ray(..),

-- *** Intersection
Intersection(..),
epsilon
) where

import HaskRay.Vector

import Data.List (minimumBy)

-- | Semi-infinate ray.
data Ray = Ray { rorigin :: !Vec3, rdir :: !Vec3 }
    deriving (Show, Read, Eq)

-- | Records an intersection with geometry.
data Intersection = Intersection { ipos :: !Vec3, inorm :: !Vec3, iray :: !Ray }
    deriving (Show, Read, Eq)

{-
Minimum distance for ray intersection to be considered important.
This is due to limited numerical precision with floating point numbers. A ray could be reflected and then be immediately registered as intersecting with the object it just bounced off.
-}
epsilon :: Scalar
epsilon = 0.001
