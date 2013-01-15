module HaskRay.Material where

import HaskRay.Vector

-- | Type alias for RGB colours.
type Colour = Vec3

averageColour :: [Colour] -> Colour
averageColour cs = scale (1/(fromIntegral $ length cs)) $ addColours cs
	where
		addColours = foldr add (Vector3 0 0 0)

multColour :: Colour -> Colour -> Colour
multColour (Vector3 r1 g1 b1) (Vector3 r2 g2 b2) = Vector3 (r1 * r2) (g1 * g2) (b1 * b2)

addColour :: Colour -> Colour -> Colour
addColour (Vector3 r1 g1 b1) (Vector3 r2 g2 b2) = Vector3 (r1 + r2) (g1 + g2) (b1 + b2)

clampColour :: Colour -> Colour
clampColour = fmap (\x -> if x < 0 then 0 else (if x > 1 then 1 else x))

correctColour :: Double -> Int
correctColour n = floor $ ((clamp n) ** (1/2.2)) * 255 + 0.5
    where
        clamp n = if n < 0 then 0 else (if n > 1 then 1 else n)

-- | Defines surface properties.
data Material = Diffuse Colour | Emissive Colour Scalar | Reflective | Transmissive Double Double deriving (Show, Read, Eq)
