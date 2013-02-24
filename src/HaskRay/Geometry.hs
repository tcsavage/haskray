module HaskRay.Geometry
(
-- ** Rays and Intersections
Ray(..),
Intersection(..),
closestIntersection,

-- ** Shapes
Shape(..),
Plane(..),
Sphere(..),
Vertex(..),
Triangle(..),
Mesh(..),

-- ** Objects
Object(..),

-- *** Predicates
isEmissive,
filterSphere,

-- *** Shape Operation Wrappers
intersectOb,
closestIntersectOb,
centerOb,
isInfiniteOb,
boundingBoxOb,
mapTextureOb,

-- ** Bounding Box
BoundingBox(..),
minPoint,
maxPoint,
minmaxPoints,
boundObjects,
intersectBox,
intersectBoxes
) where

import HaskRay.Geometry.BoundingBox
import HaskRay.Geometry.Classes
import HaskRay.Geometry.Mesh
import HaskRay.Geometry.Object
import HaskRay.Geometry.Plane
import HaskRay.Geometry.Ray
import HaskRay.Geometry.Sphere
