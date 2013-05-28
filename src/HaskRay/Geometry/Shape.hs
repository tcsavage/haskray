module HaskRay.Geometry.Shape
(
-- *** Shape Type Class
Shape(..),
closestIntersect,
closestIntersection,
boundShapes
) where

import HaskRay.Vector
import HaskRay.Material
import HaskRay.Geometry.BoundingBox
import HaskRay.Ray
import HaskRay.Monad

import Control.Monad (join)
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (minimumBy)

-- | Something renderable.
data Shape = Shape {
    -- | Find a list of intersections (and distances) between a ray and a shape.
    intersect :: Ray -> Maybe (Scalar, Intersection, Material () (BSDF Colour)),
    -- | Gets the center of the shape.
    center :: Vec3,
    -- | Calculates the axially-aligned bounding box of the shape.
    boundingBox :: Maybe BoundingBox,
    -- | Project a texture onto the shape's surface.
    mapTexture :: Vec3 -> Vec2,
    -- | Is the shape emissive.
    emissiveShape :: Bool,
    -- | Calculate a random direction for sampling the shape.
    randomSampleDir :: Vec3 -> Render Vec3
}

-- | Find closest intersection with a list of 'Object's.
closestIntersect :: [Shape] -> Ray -> Maybe (Scalar, Intersection, Material () (BSDF Colour))
closestIntersect [] _ = Nothing
closestIntersect shapes r = closestIntersection $ mapMaybe (`intersect` r) shapes

-- | Get the bounding box of several shapes.
boundShapes :: [Shape] -> BoundingBox
boundShapes os = BoundingBox ev1 ev2
    where
        bbs = join $ mapMaybe (fmap boundingPoints . boundingBox) os
        boundingPoints (BoundingBox v1 v2) = [v1, v2]
        (ev1, ev2) = minmaxPoints bbs

-- | Grabs the intersection with the shortest distance.
closestIntersection :: [(Scalar, Intersection, Material () (BSDF Colour))] -> Maybe (Scalar, Intersection, Material () (BSDF Colour))
closestIntersection [] = Nothing
closestIntersection is = Just $ minimumBy (\(d1,_,_) (d2,_,_) -> compare d1 d2) is
