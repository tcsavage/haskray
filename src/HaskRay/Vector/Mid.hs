module HaskRay.Vector.Mid where

class Vector v where
	dot :: (Floating a, Eq a) => v a -> v a -> a
	scale :: (Floating a, Eq a) => a -> v a -> v a
	magSquared :: (Floating a, Eq a) => v a -> a
	mag :: (Floating a, Eq a) => v a -> a
	neg :: (Floating a, Eq a) => v a -> v a
	add :: (Floating a, Eq a) => v a -> v a -> v a
	sub :: (Floating a, Eq a) => v a -> v a -> v a
	normalize :: (Floating a, Eq a) => v a -> v a

data Vector3 a = Vector3 a a a deriving (Show, Read, Eq)

instance Functor Vector3 where
	fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

instance Vector Vector3 where
	dot (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = (a1 * b1) + (a2 * b2) + (a3 * b3)
	scale fac = fmap (*fac)
	magSquared (Vector3 x y z) = x*x + y*y + z*z
	mag = sqrt . magSquared
	neg = fmap negate
	add (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
	sub (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)
	normalize vec
		| vec == Vector3 0 0 0 = vec
		| otherwise = scale (1 / mag vec) vec

type Vec3 = Vector3 Double

-- | Cross product is only defined for 3 and 7 dimensional vectors.
cross :: (Floating a, Eq a) => Vector3 a -> Vector3 a -> Vector3 a
cross (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
