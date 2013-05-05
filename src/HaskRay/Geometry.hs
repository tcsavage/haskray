module HaskRay.Geometry
(
-- ** Shapes
Shape(..),
Plane(..),
mkPlaneShape,
Sphere(..),
mkSphereShape,
--Vertex(..),
--Triangle(..),
--Mesh(..),

-- ** Bounding Box
BoundingBox(..),
minPoint,
maxPoint,
minmaxPoints,
boundShapes,
intersectBox,
intersectBoxes,
closestIntersect
) where

import HaskRay.Geometry.BoundingBox
--import HaskRay.Geometry.Mesh
import HaskRay.Geometry.Plane
import HaskRay.Geometry.Sphere
import HaskRay.Geometry.Shape
