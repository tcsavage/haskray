{-|
This module exposes 'Octree' and related functions. The octree is used to optimise rendering - particularly polygonal rendering.
-}
module HaskRay.Octree
(
Octree(..),
mkOctree,
ObjectStructure,
mkObStruct,
closestIntersectObStruct,
getObjects,
getAll
) where

import HaskRay.Vector
import HaskRay.Geometry
import HaskRay.Material
import HaskRay.Ray

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe

{-|
Octree type.
Nodes are referenced from top-down perspective, north-west high/low => nwh/nwl etc.
-}
data Octree = Leaf !Vec3 !Scalar ![Shape]
            | Branch { pos :: !Vec3, size :: !Scalar, nwh, nwl, neh, nel, swh, swl, seh, sel :: !Octree }

-- | List of all infinite objects, octree and all objects together.
type ObjectStructure = ([Shape], Octree, [Shape])

-- | Find closest intersection in object structure.
closestIntersectObStruct :: ObjectStructure -> Ray -> Maybe (Scalar, Intersection, Material () (BSDF Colour))
closestIntersectObStruct (inf, oct, _) ray = closest closestInf closestFin
    where
        closest Nothing x = x
        closest x Nothing = x
        closest x@(Just (d1,_,_)) y@(Just (d2,_,_)) = if d1 < d2 then x else y
        closestInf = closestIntersect inf ray
        closestFin = closestIntersectOct oct ray

-- | Maximum depth of octree.
maxDepth :: Int
maxDepth = 3

-- | Build an octree-optimised object structure (infinite objects, finite object octree) from a list of objects.
mkObStruct :: [Shape] -> ObjectStructure
mkObStruct obs = (inf, mkOctree fin, obs)
    where
        (inf, fin) = partition (\s -> boundingBox s == Nothing) obs

-- | Build an octree from a list of Objects.
mkOctree :: [Shape] -> Octree
mkOctree os = reduceLeaves 0 $ Leaf center width os
    where
        objbbs = boundShapes os
        center = boundingCubeCenter objbbs
        width = boundingCubeSize objbbs

-- | Calculate size of bounding box.
boundingCubeSize :: BoundingBox -> Scalar
boundingCubeSize (BoundingBox (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) = maximum [abs $ x1-x2, abs $ y1-y2, abs $ z1-z2]

-- | Calculate center of bounding box.
boundingCubeCenter :: BoundingBox -> Vec3
boundingCubeCenter (BoundingBox v1 v2) = scale 0.5 (v1 `add` v2)

-- | Generate all possible directions. ALways returns list length 8.
permuteDirs :: [Vec3]
permuteDirs = map (\[x,y,z] -> Vector3 x y z) permutations
    where
        permutations = replicateM 3 [1, -1]

-- | Generate a new octree configuration from an origin and size.
newConf :: Vec3 -> Scalar -> [(Vec3, Scalar)]
newConf pos size = map (\v -> (add pos $ (size/4) `scale` v, size')) permuteDirs
    where
        size' = size/2

-- Reduces the number of objects in each leaf by deepening the octree.
reduceLeaves :: Int -> Octree -> Octree
reduceLeaves n (Leaf pos size os)
    | n < 0 = error "HaskRay.Octree.reduceLeaves: Negative depth."
    | n > maxDepth = error "HaskRay.Octree.reduceLeaves: Depth too high."
    | n < maxDepth && length os > 1 = Branch pos size nwh nwl neh nel swh swl seh sel
    | otherwise = Leaf pos size os
    where
        getObs = sortObjects os
        [(nwh_p, nwh_s), (nwl_p, nwl_s), (neh_p, neh_s), (nel_p, nel_s), (swh_p, swh_s), (swl_p, swl_s), (seh_p, seh_s), (sel_p, sel_s)] = newConf pos size
        nwh = reduceLeaves (n+1) $ Leaf nwh_p nwh_s (getObs nwh_p nwh_s)
        nwl = reduceLeaves (n+1) $ Leaf nwl_p nwl_s (getObs nwl_p nwl_s)
        neh = reduceLeaves (n+1) $ Leaf neh_p neh_s (getObs neh_p neh_s)
        nel = reduceLeaves (n+1) $ Leaf nel_p nel_s (getObs nel_p nel_s)
        swh = reduceLeaves (n+1) $ Leaf swh_p swh_s (getObs swh_p swh_s)
        swl = reduceLeaves (n+1) $ Leaf swl_p swl_s (getObs swl_p swl_s)
        seh = reduceLeaves (n+1) $ Leaf seh_p seh_s (getObs seh_p seh_s)
        sel = reduceLeaves (n+1) $ Leaf sel_p sel_s (getObs sel_p sel_s)
reduceLeaves _ branch = branch

-- | Get a bounding box from an octree.
bbFromOctree :: Octree -> BoundingBox
bbFromOctree (Leaf pos size _) = bbFromBranch pos size
bbFromOctree (Branch pos size _ _ _ _ _ _ _ _) = bbFromBranch pos size

-- | Build a bounding box from an octree branch.
bbFromBranch :: Vec3 -> Scalar -> BoundingBox
bbFromBranch pos size = BoundingBox (pos `add` dif) (pos `add` neg dif)
    where
        dif = pure $ size/2

-- | Sort objects into octree membership groups.
sortObjects :: [Shape] -> Vec3 -> Scalar -> [Shape]
sortObjects os pos size = filter (intersectBoxes (bbFromBranch pos size) . fromJust . boundingBox) $ filter (\s -> boundingBox s /= Nothing) os

-- | Get all objects from sectors the ray passes through.
filterObsByIntersection :: Octree -> Ray -> [Shape]
filterObsByIntersection (Leaf pos size obs) ray
    | intersectBox ray $ bbFromBranch pos size = obs
    | otherwise = []
filterObsByIntersection (Branch pos size nwh nwl neh nel swh swl seh sel) ray
    | intersectBox ray $ bbFromBranch pos size = concatMap (`filterObsByIntersection` ray) [nwh, nwl, neh, nel, swh, swl, seh, sel]
    | otherwise = []

-- | Get all objects inside an object structure which may possibly intersect with a ray..
getObjects :: ObjectStructure -> Ray -> [Shape]
getObjects (inf, oct, _) ray = inf ++ filterObsByIntersection oct ray

-- | Get all objects inside an object structure.
getAll :: ObjectStructure -> [Shape]
getAll (_, _, os) = os

-- | Find the closest intersection with an object structure.
closestIntersectOct :: Octree -> Ray -> Maybe (Scalar, Intersection, Material () (BSDF Colour))
closestIntersectOct octree ray = closestIntersect (filterObsByIntersection octree ray) ray
