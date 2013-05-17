module HaskRay.Geometry.Sphere
(
Sphere(..),
mkSphereShape
) where

import HaskRay.Vector
import HaskRay.Geometry.BoundingBox
import HaskRay.Geometry.Shape
import HaskRay.Ray
import HaskRay.Geometry.Util
import HaskRay.Material
import HaskRay.Monad

import Control.Applicative
import Data.Maybe
import Data.Typeable

{-|
Defines a mathematical sphere.
-}
data Sphere = Sphere !Vec3 !Scalar deriving (Show, Read, Eq)

mkSphereShape :: Sphere -> Material () (BSDF Colour) -> Shape
mkSphereShape s m = Shape { intersect = intersect s m, center = center s, boundingBox = boundingBox s, mapTexture = mapTexture s, emissiveShape = isEmissive m, randomSampleDir = randomSampleDir s }
    where
        intersect s@(Sphere center rad) material ray@(Ray origin dir) = is
            where
                a = magSquared dir
                b = 2 * ( dir `dot` (origin `sub` center))
                c = magSquared (origin `sub` center) - rad*rad
                times = filter (> epsilon) (roots a b c)
                normal_at_time t = normalize (positionAtTime ray t `sub` center)
                intersection_at_time t = Intersection (positionAtTime ray t) (normal_at_time t) ray
                is = closestIntersection $ map (\t -> (t,intersection_at_time t, material)) times
        center (Sphere c _) = c
        boundingBox (Sphere c r) = Just $ BoundingBox (c `add` pure r) (c `sub` pure r)
        mapTexture (Sphere center _) p = Vector2 u v
            where
                d = normalize (center `sub` p)
                u = 0.5 + (atan2 (z3 d) (x3 d) / (2*pi))
                v = 0.5 - 2 * (asin (y3 d) / (2*pi))
        randomSampleDir (Sphere c r) pos = do
            eps1 <- getRandomR (0, 1)
            eps2 <- getRandomR (0, 1)
            let sw = c `sub` pos
            let su = normalize (if abs (x3 sw) > 1 then Vector3 0 1 0 else Vector3 1 0 0)
            let sv = sw `cross` su
            let cosAMax = sqrt (1 - r * r / ((pos `sub` c) `dot` (pos `sub` c)))
            let cosA = 1 - eps1 + (eps1 * cosAMax)
            let sinA = sqrt $ 1 - cosA * cosA
            let phi = 2 * pi * eps2
            return $ normalize $! scale (cos phi * sinA) su `add` scale (sin phi * sinA) sv `add` scale cosA sw
            --return $ normalize (c `sub` pos) -- TEMPORARY
