module HaskRay.RayTree.String where

import HaskRay.RayTree
import HaskRay.Vector
import HaskRay.Material
import Text.Printf
import Data.Tree

treeString :: Pixel -> String
treeString (Pixel samples) = drawTree $ Node "Pixel" (map sample2tree samples)

colStr :: Colour -> String
colStr colour = printf "Colour (%.3f, %.3f, %.3f)" (x3 colour) (y3 colour) (z3 colour)

sample2tree :: Sample -> Tree String
sample2tree (Diff colour shadows gi) = Node "Diffuse" $ (Node (colStr colour) []) : (Node "Global Illumination" [sample2tree gi]) : (map shadow2tree shadows)
sample2tree (Emm colour) = Node "Emissive" [Node (colStr colour) []]
sample2tree (Reflection sample) = Node "Reflection" [sample2tree sample]
sample2tree (Refraction samplet sampler mix) = Node (printf "Refraction (Mix: %.3f)" mix) [sample2tree samplet, sample2tree sampler]
sample2tree s = Node (show s) []

shadow2tree :: Shadow -> Tree String
shadow2tree (Shadow colour) = Node "Shadow" [Node (colStr colour) []]
