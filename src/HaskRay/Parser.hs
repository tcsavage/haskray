{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, RankNTypes #-}

module HaskRay.Parser
(
serialize,
deserialize
) where

import HaskRay.Material
import HaskRay.Geometry
import HaskRay.Scene
import HaskRay.Projection
import HaskRay.Vector
import Control.Monad
import Data.Typeable

data SceneRep = SceneRep [ObjectRep] View deriving (Show, Read)

data ObjectRep = ObjectRep ShapeRep MaterialRep deriving (Show, Read)

data ShapeRep = ShapeRep String String deriving (Show, Read)

data MaterialRep = ShadedRep DiffuseRep
                 | ShadelessRep DiffuseRep
                 | EmissiveRep Colour Scalar
                 | ReflectiveRep
                 | TransmissiveRep Scalar Scalar
                 deriving (Show, Read)

data DiffuseRep = FlatRep Colour | TexturedRep String deriving (Show, Read)

serializeDiffuse :: Diffuse -> DiffuseRep
serializeDiffuse (Flat colour) = FlatRep colour
serializeDiffuse (Textured texture) = TexturedRep "sometexture.bmp"

deserializeDiffuse :: DiffuseRep -> IO Diffuse
deserializeDiffuse (FlatRep colour) = return $ Flat colour
deserializeDiffuse (TexturedRep path) = do
    texture <- loadTexture path
    return $ Textured texture

serializeMaterial :: Material -> MaterialRep
serializeMaterial (Shaded diffuse) = ShadedRep $ serializeDiffuse diffuse
serializeMaterial (Shadeless diffuse) = ShadelessRep $ serializeDiffuse diffuse
serializeMaterial (Emissive colour power) = EmissiveRep colour power
serializeMaterial Reflective = ReflectiveRep
serializeMaterial (Transmissive i r) = TransmissiveRep i r

deserializeMaterial :: MaterialRep -> IO Material
deserializeMaterial (ShadedRep diffuse) = liftM Shaded (deserializeDiffuse diffuse)
deserializeMaterial (ShadelessRep diffuse) = liftM Shadeless (deserializeDiffuse diffuse)
deserializeMaterial (EmissiveRep colour power) = return $ Emissive colour power
deserializeMaterial ReflectiveRep = return $ Reflective
deserializeMaterial (TransmissiveRep i r) = return $ Transmissive i r

serializeObject :: Object -> ObjectRep
serializeObject (Object s m) = ObjectRep (ShapeRep (show $ typeOf s) (show s)) $ serializeMaterial m

deserializeObject :: ObjectRep -> IO Object
deserializeObject (ObjectRep (ShapeRep "Sphere" rep) mr) = deserializeMaterial mr >>= \m -> return $ Object (read rep :: Sphere) m
deserializeObject (ObjectRep (ShapeRep "Plane" rep) mr) = deserializeMaterial mr >>= \m -> return $ Object (read rep :: Plane) m
deserializeObject (ObjectRep (ShapeRep "Mesh" rep) mr) = deserializeMaterial mr >>= \m -> return $ Object (read rep :: Mesh) m
deserializeObject _ = error "HaskRay.Parser.deserializeObject: Unknown shape."

serialzeScene :: Scene -> SceneRep
serialzeScene (Scene objs view) = SceneRep (map serializeObject objs) view

deserializeScene :: SceneRep -> IO Scene
deserializeScene (SceneRep objrs view) = do
    objs <- mapM deserializeObject objrs
    return $ Scene objs view

serialize :: Scene -> String
serialize = show . serialzeScene

deserialize :: String -> IO Scene
deserialize = deserializeScene . read
