{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (foldr)
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function
import Data.Foldable
import Control.Applicative
import Control.Monad

import Debug.Trace
import Text.Printf

import HaskRay.Vector

-- Fudge factor for aparoximate equality.
fudge :: Double
fudge = 0.000001

-- Some properties will hold true but produce slightly different results due to floating-point precision errors. (~=) defines an "approximately equal test".
class ProxTest a where
	(~=) :: a -> a -> Bool

instance ProxTest Double where
	d1 ~= d2 = (abs $ d1-d2) < fudge

instance ProxTest a => ProxTest (Vector3 a) where
	v1 ~= v2 = foldr (&&) True $ (~=) <$> v1 <*> v2

prop_vector_functorlaws_1 :: Double -> Double -> Double -> Bool
prop_vector_functorlaws_1 x1 y1 z1 = fmap id v1 == v1
	where
		v1 = Vector3 x1 y1 z1

prop_vector_functorlaws_2 :: Double -> Double -> Double -> Bool
prop_vector_functorlaws_2 x1 y1 z1 = fmap (f . g) v1 == (fmap f . fmap g) v1
	where
		v1 = Vector3 x1 y1 z1
		f = (*2)
		g = (+1)

prop_vector_applicativelaws_1 :: Double -> Double -> Double -> Bool
prop_vector_applicativelaws_1 x1 y1 z1 = (pure id <*> v1) == v1
	where
		v1 = Vector3 x1 y1 z1

prop_vector_applicativelaws_2 :: Double -> Bool
prop_vector_applicativelaws_2 x1 = (pure f <*> pure x1) == (pure (f x1) :: Vec3)
	where
		f = (*2)

prop_testMagSq :: Double -> Bool
prop_testMagSq x = magSquared (Vector3 x 0 0) == x**2

prop_vec3AddScale :: Double -> Double -> Double -> Bool
prop_vec3AddScale x1 y1 z1 = (add v1 v1) == (scale 2 v1)
	where
		v1 = Vector3 x1 y1 z1

prop_vec3_dot_1 :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
prop_vec3_dot_1 x1 y1 z1 x2 y2 z2 = (v1 `dot` v2) == (v2 `dot` v1)
	where
		v1 = Vector3 x1 y1 z1
		v2 = Vector3 x2 y2 z2

prop_vec3_dot_2 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
prop_vec3_dot_2 x1 y1 z1 x2 y2 z2 x3 y3 z3 = (v1 `dot` (v2 `add` v3)) ~= ((v1 `dot` v2) + (v1 `dot` v3))
	where
		v1 = Vector3 x1 y1 z1
		v2 = Vector3 x2 y2 z2
		v3 = Vector3 x3 y3 z3

prop_vec3_dot_3 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
prop_vec3_dot_3 c x1 y1 z1 x2 y2 z2 = t1 ~= ((c `scale` v1) `dot` v2) && t1 ~= (v1 `dot` (c `scale` v2))
	where
		v1 = Vector3 x1 y1 z1
		v2 = Vector3 x2 y2 z2
		t1 = c * (v1 `dot` v2)

prop_vec3_dot_4 :: Double -> Double -> Double -> Bool
prop_vec3_dot_4 x1 y1 z1 = (v1 `dot` v1) == magSquared v1
	where
		v1 = Vector3 x1 y1 z1

prop_vec3_dot_5 :: Double -> Double -> Double -> Bool
prop_vec3_dot_5 x1 y1 z1 = (v1 `dot` v1) >= 0
	where
		v1 = Vector3 x1 y1 z1

prop_vec3_dot_6 :: Bool
prop_vec3_dot_6 = (v1 `dot` v1) == 0
	where
		v1 = vzero :: Vec3

prop_vec3_cross_1 :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
prop_vec3_cross_1 x1 y1 z1 x2 y2 z2 = (v1 `cross` v2) == neg (v2 `cross` v1)
	where
		v1 = Vector3 x1 y1 z1
		v2 = Vector3 x2 y2 z2

prop_vec3_cross_2 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
prop_vec3_cross_2 x1 y1 z1 x2 y2 z2 x3 y3 z3 = (v1 `cross` (v2 `add` v3)) ~= ((v1 `cross` v2) `add` (v1 `cross` v3))
	where
		v1 = Vector3 x1 y1 z1
		v2 = Vector3 x2 y2 z2
		v3 = Vector3 x3 y3 z3

prop_vec3_cross_3 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
prop_vec3_cross_3 c x1 y1 z1 x2 y2 z2 = ((c `scale` v1) `cross` v2) ~= t1 && t1 ~= (v1 `cross` (c `scale` v2))
	where
		v1 = Vector3 x1 y1 z1
		v2 = Vector3 x2 y2 z2
		t1 = c `scale` (v1 `cross` v2)

prop_vec3_cross_4 :: Double -> Double -> Double -> Bool
prop_vec3_cross_4 x1 y1 z1 = (v1 `cross` vzero) == vzero && (vzero `cross` v1) == vzero
	where
		v1 = Vector3 x1 y1 z1

prop_vec3_cross_5 :: Double -> Double -> Double -> Bool
prop_vec3_cross_5 x1 y1 z1 = (v1 `cross` v1) == vzero
	where
		v1 = Vector3 x1 y1 z1

prop_vec3_cross_6 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
prop_vec3_cross_6 x1 y1 z1 x2 y2 z2 x3 y3 z3 = (v1 `dot` (v2 `cross` v3)) ~= ((v1 `cross` v2) `dot` v3)
	where
		v1 = Vector3 x1 y1 z1
		v2 = Vector3 x2 y2 z2
		v3 = Vector3 x3 y3 z3

main :: IO ()
main = do
	b <- $quickCheckAll
	unless b $ exitWith (ExitFailure 1)
