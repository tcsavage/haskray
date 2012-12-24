module HaskRay.Vector.Simple where

data Vector3 a = Vector3 a a a deriving (Show, Read, Eq)

type Vec3 = Vector3 Double

dot :: (Floating a, Eq a) => Vector3 a -> Vector3 a -> a
dot (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = (a1 * b1) + (a2 * b2) + (a3 * b3)

cross :: (Floating a, Eq a) => Vector3 a -> Vector3 a -> Vector3 a
cross (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)

scale :: (Floating a, Eq a) => a -> Vector3 a -> Vector3 a
scale fac (Vector3 x y z) = Vector3 (x*fac) (y*fac) (z*fac)

magSquared :: (Floating a, Eq a) => Vector3 a -> a
magSquared (Vector3 x y z) = x*x + y*y + z*z

mag :: (Floating a, Eq a) => Vector3 a -> a
mag = sqrt . magSquared

neg :: (Floating a, Eq a) => Vector3 a -> Vector3 a
neg (Vector3 x y z) = Vector3 (-x) (-y) (-z)

add :: (Floating a, Eq a) => Vector3 a -> Vector3 a -> Vector3 a
add (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)

sub :: (Floating a, Eq a) => Vector3 a -> Vector3 a -> Vector3 a
sub (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)

normalize :: (Floating a, Eq a) => Vector3 a -> Vector3 a
normalize vec
	| vec == Vector3 0 0 0 = vec
	| otherwise = scale (1 / mag vec) vec
