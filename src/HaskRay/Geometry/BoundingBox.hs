module HaskRay.Geometry.BoundingBox
(
	BoundingBox(..),

	maxPoint,
	minPoint,
	minmaxPoints,
	intersectBox,
	intersectBoxes
) where

import HaskRay.Vector
import HaskRay.Geometry.Ray

import Control.Applicative

data BoundingBox = BoundingBox Vec3 Vec3 deriving (Show, Eq)

-- | Get the maximum point from a list of vectors. Useful for building bounding boxes.
maxPoint :: [Vec3] -> Vec3
maxPoint = foldr1 (\a b -> max <$> a <*> b)

-- | Get the minimum point from a list of vectors. Useful for building bounding boxes.
minPoint :: [Vec3] -> Vec3
minPoint = foldr1 (\a b -> min <$> a <*> b)

-- | Get the minimum and maximum points from a list of vectors.
minmaxPoints :: [Vec3] -> (Vec3, Vec3)
minmaxPoints (x:xs) = foldr (\b (cmin, cmax) -> (min <$> cmin <*> b, max <$> cmax <*> b)) (x, x) xs

-- | Find out whether a point is inside a box.
pointInBox :: BoundingBox -> Vec3 -> Bool
pointInBox (BoundingBox (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) (Vector3 x y z) = (maximum [x1, x2, x] /= x) && (maximum [y1, y2, y] /= y) && (maximum [z1, z2, z] /= z)

-- | Perform ray intersection on a box.
intersectBox :: Ray -> BoundingBox -> Bool
intersectBox (Ray (Vector3 ox oy oz) d) (BoundingBox v1 v2) = not (tmax < 0 || tmin > tmax)
	where
		(Vector3 dfx dfy dfz) = (1/) <$> d
		(Vector3 lbx lby lbz) = min <$> v1 <*> v2
		(Vector3 rtx rty rtz) = max <$> v1 <*> v2
		t1 = (lbx - ox) * dfx
		t2 = (rtx - ox) * dfx
		t3 = (lby - oy) * dfy
		t4 = (rty - oy) * dfy
		t5 = (lbz - oz) * dfz
		t6 = (rtz - oz) * dfz
		tmin = minimum [t1, t2, t3, t4, t5, t6]
		tmax = maximum [t1, t2, t3, t4, t5, t6]

-- | Perform box-box intersection.
intersectBoxes :: BoundingBox -> BoundingBox -> Bool
intersectBoxes (BoundingBox (Vector3 x11 y11 z11) (Vector3 x12 y12 z12)) (BoundingBox (Vector3 x21 y21 z21) (Vector3 x22 y22 z22)) = and clst
	where
		max1x = max x11 x12
		max1y = max y11 y12
		max1z = max z11 z12
		min1x = min x11 x12
		min1y = min y11 y12
		min1z = min z11 z12
		max2x = max x21 x22
		max2y = max y21 y22
		max2z = max z21 z22
		min2x = min x21 x22
		min2y = min y21 y22
		min2z = min z21 z22
		clst =
			[max1x > min2x
			,min1x < max2x
			,max1y > min2y
			,min1y < max2y
			,max1z > min2z
			,min1z < max2z]
