module HaskRay.Projection
(View(..)
,makeCameraRays
) where

import HaskRay.Vector
import HaskRay.Geometry
import HaskRay.Settings
import System.Random

-- | Camera type.
data View = View { position :: Vec3, lookAt :: Vec3, upVec :: Vec3, fov :: Scalar } deriving (Show, Read, Eq)

-- | Generate rays from camera and pixel grid.
perspectiveProjection :: View -> Vec3 -> Ray
perspectiveProjection (View camerapos _ _ _) point = Ray point (normalize (point `sub` camerapos))

-- | Generate rays from camera and pixel grid.
parallelProjection :: View -> Vec3 -> Ray
parallelProjection (View camerapos lookingat _ _) point = Ray point (normalize (lookingat `sub` camerapos))

-- | Generate a grid of pixels.
genGrid :: Scalar -> Scalar -> [(Scalar, Scalar)]
genGrid w h = [(x, y) | y <- [0..h-1], x <- [0..w-1]]

-- | Apply a random distribution to a pixel to create sample list.
sampleizePixel :: [(Scalar, Scalar)] -> (Scalar, Scalar) -> [(Scalar, Scalar)]
sampleizePixel sampGrid (x, y) = map (\(sx, sy) -> (x+sx, y+sy)) sampGrid

-- | Generate the list of points on the image plane.
pixelGrid :: [(Scalar, Scalar)] -- ^ Random distribution
              -> View -- ^ Camera
              -> Scalar -- ^ Image width
              -> Scalar -- ^ Image height
              -> [[Vec3]]
pixelGrid randoms (View cameraPos lookingAt viewUp fov) width height = map (map transform) grid
	where
		aspectRatio = height/width
		fovx = fov * (pi / 180) -- Convert degrees to radians. Use fovx for trig operations
		fovy = aspectRatio * fovx
		tfovx = tan fovx
		tfovy = tan fovy
		viewDir = normalize (lookingAt `sub` cameraPos)
		viewRight = viewDir `cross` viewUp
		gridCenter = cameraPos `add` viewDir
		adj t c = (c / t) - 0.5
		grid = map (map (\(x, y) -> (tfovx * adj width x, tfovy * adj height y))) $ map (sampleizePixel randoms) (genGrid width height)
		transform (x, y) = gridCenter `add` (scale x viewRight) `add` (scale y (neg viewUp))

-- | Generate a random distribution to use in creating multi-sampled pixels.
genRandomOffsets :: Int -- ^ Number of samples
                 -> StdGen -- ^ Random seed
                 -> [(Scalar, Scalar)]
genRandomOffsets samp rand = pair rands
	where
		rands = take (samp * 2) $ randomRs (-0.5, 0.5) rand
		pair [] = []
		pair (a:b:x) = (a,b):(pair x)
		pair _ = error "Non-even list (for some reason)."

-- | Generate pixel rays for the camera.
makeCameraRays :: Settings -> View -> [[Ray]]
makeCameraRays (Settings w h samples rand) view = map (map (perspectiveProjection view)) (pixelGrid (genRandomOffsets samples rand) view (fromIntegral w) (fromIntegral h))
