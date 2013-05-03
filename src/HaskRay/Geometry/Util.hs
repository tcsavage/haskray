module HaskRay.Geometry.Util where

import HaskRay.Vector
import HaskRay.Ray

-- | Find the position of a point a specified distance along a ray.
positionAtTime :: Ray -> Scalar -> Vec3
positionAtTime (Ray origin dir) t = origin `add` scale t dir

-- | Solves the quadratic formula for a given a, b and c; where ax^2 + bx + c = 0.
roots :: Scalar -> Scalar -> Scalar -> [Scalar]
roots a b c
    | discriminant < 0 = []
    | otherwise = [0.5 * (-b + sqrt discriminant), 0.5 * (-b - sqrt discriminant)]
    where
        discriminant = b*b - 4*a*c
