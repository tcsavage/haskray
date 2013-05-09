{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
-- Using -XFlexibleInstances we can define a Monoid instance for all instanced of Vector.
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- Automatically add an UNPACK pragma for strict records.

module HaskRay.Vector.Advanced
(
-- ** Vector Class
Vector(..),
-- ** Vector3 Type
Vector3(..),
cross,
-- ** Vector2 Type
Vector2(..),
-- ** Type Aliases
Scalar,
Vec3,
Vec2,
-- * Foldable Methods
fold,
-- * Monoid Methods
mempty,
mappend,
mconcat,
(<>)
) where

import Prelude hiding (foldr, sum)
import Control.Applicative
import Control.DeepSeq
import Data.Foldable
import Data.Monoid

-- | Typeclass for vector types.
class Vector v where
    -- | Zero vector. Magnitude == 0.
    vzero :: (Num a) => v a
    -- | Dot product.
    dot :: (Floating a, Eq a) => v a -> v a -> a
    -- | Scalar multiplication.
    scale :: (Floating a, Eq a) => a -> v a -> v a
    -- | Magnitude squared.
    magSquared :: (Floating a, Eq a) => v a -> a
    -- | Magnitude.
    mag :: (Floating a, Eq a) => v a -> a
    mag = sqrt . magSquared
    -- | Negate.
    neg :: (Floating a, Eq a) => v a -> v a
    -- | Vector addition.
    add :: (Num a) => v a -> v a -> v a
    -- | Vector subtraction.
    sub :: (Num a) => v a -> v a -> v a
    -- | Make unit vector.
    normalize :: (Floating a, Eq a) => v a -> v a

-- | Three-dimensional vector type.
data Vector3 a = Vector3 { x3 :: !a, y3 :: !a, z3 :: !a } deriving (Show, Read, Eq)

{-
Here we define an instance of the Functor type class for Vector3.

Functor describes types that can be mapped over (such as [] or Maybe). It has a single method: 'fmap' which applies a function to each value inside the type.
In this case, we want the function applied to all three components of the Vector3.

Now we can apply functions to all components of a Vector3 very easily:
> fmap (*2) (Vector3 1 2 3) == (Vector3 (1*2) (2*2) (3*2))
-}
instance Functor Vector3 where
    fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

{-
The Applicative instance allows us to lift function application into Vector3.

For example, one can define a Vector3 with functions in the x, y and z components:
> Vector3 (*2) (+1) (*10)
These functions can then be applied to components in another Vector3 value with (<*>):
> (Vector3 (*2) (+1) (*10)) <*> (Vector3 1 2 3) == (Vector3 (1*2) (2+1) (3*10))

'pure' will lift a normal function into a Vector3:
> pure (*2) == (Vector3 (*2) (*2) (*2))

Control.Applicative also defines (<$>), which is an infix version of 'fmap'.
> fmap f (Vector3 a b c) == f <$> (Vector3 a b c) == (pure f) <*> (Vector3 a b c)
-}
instance Applicative Vector3 where
    pure a = Vector3 a a a
    (Vector3 f1 f2 f3) <*> (Vector3 a b c) = Vector3 (f1 a) (f2 b) (f3 c)

{-
Vectors are also foldable - that is, we can form a "summary" value of from a Vector value, a binary function and an initial accumulator value.
-}
instance Foldable Vector3 where
    foldr f a (Vector3 x y z) = f x (f y (f z a))

{-
Vector3 belogs in the Vector typeclass (obviously). Here we define the class methods for the instance.
-}
instance Vector Vector3 where
    vzero = pure 0
    dot a b = sum $ (*) <$> a <*> b
    scale fac = fmap (*fac)
    magSquared = sum . fmap (**2)
    neg = fmap negate
    add a b = (+) <$> a <*> b
    sub a b = (-) <$> a <*> b
    normalize vec
        | vec == vzero = vec
        | otherwise = scale (1 / mag vec) vec

-- | Cross product. Excluded from 'Vector' because it only exists for 3 and 7 dimensional vectors (Massey, 1983, p. 697).
cross :: (Floating a, Eq a) => Vector3 a -> Vector3 a -> Vector3 a
cross (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)

-- | Two-demensional vector type.
data Vector2 a = Vector2 { x2 :: !a, y2 :: !a } deriving (Show, Read, Eq)

instance Functor Vector2 where
    fmap f (Vector2 x y) = Vector2 (f x) (f y)

instance Applicative Vector2 where
    pure a = Vector2 a a 
    (Vector2 f1 f2) <*> (Vector2 a b) = Vector2 (f1 a) (f2 b)

instance Foldable Vector2 where
    foldr f a (Vector2 x y) = f x (f y a)

instance Vector Vector2 where
    vzero = pure 0
    dot a b = sum $ (*) <$> a <*> b
    scale fac = fmap (*fac)
    magSquared = sum . fmap (**2)
    neg = fmap negate
    add a b = (+) <$> a <*> b
    sub a b = (-) <$> a <*> b
    normalize vec
        | vec == vzero = vec
        | otherwise = scale (1 / mag vec) vec

{-
All vectors form a monoid under addition.
-}
instance (Vector v, Floating a, Eq a) => Monoid (v a) where
    mappend = add
    mempty = vzero

instance NFData (Vector3 a) where

-- Scalar type alias.
type Scalar = Double

-- Vector type aliases.
type Vec3 = Vector3 Scalar
type Vec2 = Vector2 Scalar
