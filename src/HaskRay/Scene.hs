module HaskRay.Scene where

import HaskRay.Geometry
import HaskRay.Projection

-- | A scene is a list of objects and a camera.
data Scene = Scene [Shape] View

optimiseScene :: Scene -> Scene
--optimiseScene (Scene os c) = Scene (expandMeshes os) c
optimiseScene = id
