{-|
This module exposes 'Octree' and related functions. The octree is used to optimise rendering - particularly polygonal rendering.
-}
module HaskRay.Octree where

import HaskRay.Vector
import HaskRay.Geometry

import Control.Applicative
import Control.Monad
import Data.List

import Debug.Trace

{-|
Octree type.
Nodes are referenced from top-down perspective, north-west high/low => nwh/nwl etc.
-}
data Octree = Leaf Vec3 Scalar [Object] | Branch { pos :: Vec3, size :: Scalar, nwh, nwl, neh, nel, swh, swl, seh, sel :: Octree } deriving (Show)

-- | List of all infinite objects, octree and all objects together.
type ObjectStructure = ([Object], Octree, [Object])

maxObs = 3

maxDepth = 3

--mkOctree :: [Object] -> Octree
--mkOctree os
--	| length os <= maxObs = Leaf vzero 1 os
--	| otherwise = Branch center width empty empty empty empty empty empty empty empty
--	where
--		objbbs = boundObjects os
--		center = boundingCubeCenter objbbs
--		width = boundingCubeSize objbbs
--		empty = Leaf vzero 1 []
--		getMembership (Branch center width _ _ _ _ _ _ _ _) o = undefined
--			where
--				(BoundingBox p1 p2) = boundingBoxOb o
--		mkSubTree os = undefined

-- | Build an octree-optimised object structure (infinite objects, finite object octree) from a list of objects.
mkObStruct :: [Object] -> ObjectStructure
mkObStruct obs = (inf, mkOctree fin, obs)
	where
		(inf, fin) = partition isInfiniteOb obs

-- | Build an octree from a list of Objects.
mkOctree :: [Object] -> Octree
mkOctree os = reduceLeaves 0 $ Leaf center width os
	where
		objbbs = boundObjects os
		center = boundingCubeCenter objbbs
		width = boundingCubeSize objbbs

boundingCubeSize :: BoundingBox -> Scalar
boundingCubeSize (BoundingBox (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) = maximum [abs $ x1-x2, abs $ y1-y2, abs $ z1-z2]

boundingCubeCenter :: BoundingBox -> Vec3
boundingCubeCenter (BoundingBox v1 v2) = scale 0.5 (v1 `add` v2)

-- Generate all possible directions. ALways returns list length 8.
permuteDirs :: [Vec3]
permuteDirs = map (\[x,y,z] -> Vector3 x y z) permutations
	where
		permutations = replicateM 3 [1, -1]

newConf :: Vec3 -> Scalar -> [(Vec3, Scalar)]
newConf pos size = map (\v -> (add pos $ (size/4) `scale` v, size')) permuteDirs
	where
		size' = size/2

-- Reduces the number of objects in each leaf by deepening the octree.
reduceLeaves :: Int -> Octree -> Octree
reduceLeaves n (Leaf pos size os)
	| n < 0 = error "Negative depth"
	| n > maxDepth = error "Depth too high"
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

bbFromOctree :: Octree -> BoundingBox
bbFromOctree (Leaf pos size _) = bbFromBranch pos size
bbFromOctree (Branch pos size _ _ _ _ _ _ _ _) = bbFromBranch pos size

bbFromBranch :: Vec3 -> Scalar -> BoundingBox
bbFromBranch pos size = BoundingBox (pos `add` dif) (pos `add` (neg dif))
	where
		dif = pure $ size/2

-- Sorts objects into octree membership groups.
sortObjects :: [Object] -> Vec3 -> Scalar -> [Object]
sortObjects os pos size = filter ((intersectBoxes (bbFromBranch pos size)) . boundingBoxOb) $ filter (not . isInfiniteOb) os

-- Get all objects from sectors the ray passes through.
filterObsByIntersection :: Octree -> Ray -> [Object]
filterObsByIntersection (Leaf pos size obs) ray
	| intersectBox ray $ bbFromBranch pos size = obs
	| otherwise = []
filterObsByIntersection (Branch pos size nwh nwl neh nel swh swl seh sel) ray
	| intersectBox ray $ bbFromBranch pos size = nub $ concatMap (flip filterObsByIntersection ray) [nwh, nwl, neh, nel, swh, swl, seh, sel]
	| otherwise = []

getObjects :: ObjectStructure -> Ray -> [Object]
getObjects (inf, oct, _) ray = inf ++ (filterObsByIntersection oct ray)

getAll :: ObjectStructure -> [Object]
getAll (_, _, os) = os
--getAll (inf, oct) = inf ++ get oct
--	where
--		get (Branch _ _ a b c d e f g h) = concatMap get [a, b, c, d, e, f, g, h]
--		get (Leaf _ _ os) = os

{-
Functions:

Make Octree
Insert
Remove
Cull (frustum/occlusion)

-- | Takes an octree (leaf) and splits it down into sub trees.
simplifyLeaves :: Octree -> Octree

-- | Recursively tests for intersections between ray and octree, returning objects in leaves the ray passes through.
collectObjects :: Octree -> [Object]

-}
