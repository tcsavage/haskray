module HaskRay.Geometry.Plane
(
Plane(..),
mkPlaneShape
) where

import HaskRay.Vector
import HaskRay.Geometry.Shape
import HaskRay.Ray
import HaskRay.Geometry.Util
import HaskRay.Material

{-|
Defines an infinite plane.

> Plane (Vector3 a b c) d => ax + by + cz + d = 0
-}
data Plane = Plane !Vec3 !Scalar deriving (Show, Read, Eq)

mkPlaneShape :: Plane -> Material () (BSDF Colour) -> Shape
mkPlaneShape p m = Shape { intersect = intersect p m, center = center p, boundingBox = Nothing, mapTexture = mapTexture p, emissiveShape = isEmissive m, randomSampleDir = randomSampleDir p }
    where
        intersect s@(Plane normal d) material ray@(Ray origin dir)
            | vd == 0 = Nothing
            | t > epsilon = Just (t, Intersection hitpoint (if vd > 0 then neg normal else normal) ray, material)
            | otherwise = Nothing
            where
                vd = normalize normal `dot` dir
                v0 = negate ((normalize normal `dot` origin) + d)
                t = v0 / vd
                hitpoint = positionAtTime ray t
        center (Plane normal d) = scale d normal
        mapTexture = error "Can't texture plane (yet)"
        randomSampleDir (Plane n d) = error "Can't sample infinate plane"

