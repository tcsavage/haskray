module HaskRay.Parser
(
serialize,
deserialize
) where

import HaskRay.Material
import HaskRay.Geometry
import HaskRay.Scene
import HaskRay.Projection
import Data.Typeable

data SceneRep = SceneRep [ObjectRep] View deriving (Show, Read)

data ObjectRep = ObjectRep ShapeRep Material deriving (Show, Read)

data ShapeRep = ShapeRep String String deriving (Show, Read)

serializeObject :: Object -> ObjectRep
serializeObject (Object s m) = ObjectRep (ShapeRep (show $ typeOf s) (show s)) m

deserializeObject :: ObjectRep -> Object
deserializeObject (ObjectRep (ShapeRep "Sphere" rep) m) = Object (read rep :: Sphere) m
deserializeObject (ObjectRep (ShapeRep "Plane" rep) m) = Object (read rep :: Plane) m
deserializeObject (ObjectRep (ShapeRep "Mesh" rep) m) = Object (read rep :: Mesh) m
deserializeObject _ = error "HaskRay.Parser.deserializeObject: Unknown shape."

serialzeScene :: Scene -> SceneRep
serialzeScene (Scene objs view) = SceneRep (map serializeObject objs) view

deserializeScene :: SceneRep -> Scene
deserializeScene (SceneRep objs view) = Scene (map deserializeObject objs) view

serialize :: Scene -> String
serialize = show . serialzeScene

deserialize :: String -> Scene
deserialize = deserializeScene . read
