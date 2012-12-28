module HaskRay.Light where

import HaskRay.Vector
import HaskRay.Geometry
import HaskRay.Material
import HaskRay.Monad
import HaskRay.Octree

import Data.Typeable
import Data.Maybe
import Control.Monad.Random
import System.Random

-- TODO: Shadow through reflection/refraction
doLighting :: Vec3 -> Vec3 -> Object -> Render Scalar
doLighting x norm light@(Object ls (Emissive colour empow)) = do
    obs <- ask
    eps1 <- getRandomR (0, 1)
    eps2 <- getRandomR (0, 1)
    let sphere = fromMaybe (error "Emissive surface not sphere") $ cast ls
    let (Sphere center radius) = sphere
    let sw = center `sub` x
    let su = normalize (if (abs $ x3 sw) > 1 then Vector3 0 1 0 else Vector3 1 0 0)
    let sv = sw `cross` su
    let cosAMax = sqrt (1 - radius * radius / ((x `sub` center) `dot` (x `sub` center)))
    let cosA = 1 - eps1 + (eps1 * cosAMax)
    let sinA = sqrt $ 1 - cosA * cosA
    let phi = 2 * pi * eps2
    let l = normalize $ (scale ((cos phi) * sinA) su) `add` (scale ((sin phi) * sinA) sv) `add` (scale cosA sw)
    let ray = Ray x l
    let closestOb = closestIntersectObStruct ray obs
    --let getOb (Just (_,_,o)) = o
    --let closestObIsLight = if isNothing closestOb then True else (if (getOb closestOb) == light then True else False)
    let closestObIsLight = fromMaybe True $ closestOb >>= (\(_,_,o) -> Just $ o == light)
    let omega = 2*pi*(1-cosAMax)
    let fact = empow * (l `dot` norm) * omega
    return $ if closestObIsLight then clamp fact else 0
    where
        clamp fac = if fac < 0 then 0 else fac
        isReflectiveOrTransmissive (_, (Intersection _ _ _ (Reflective)), _) = True
        isReflectiveOrTransmissive (_, (Intersection _ _ _ (Transmissive _ _)), _) = True
        isReflectiveOrTransmissive _ = False

--getRandomPoint :: StdGen -> Vec3 -> Sphere -> (Vec3, StdGen)
--getRandomPoint rand x (Sphere center radius) = (l, rand')
--  where
--      sw = center `sub` x
--      su = normalize (if (abs $ x3 sw) > 1 then Vector3 0 1 0 else Vector3 1 0 0)
--      sv = sw `cross` su
--      cosAMax = sqrt (1 - radius * radius / ((x `sub` center) `dot` (x `sub` center)))
--      (eps1, eps2, rand') = get2Rands rand
--      cosA = 1 - eps1 + (eps1 * cosAMax)
--      sinA = sqrt $ 1 - cosA * cosA
--      phi = 2 * pi * eps2
--      l = normalize $ (scale ((cos phi) * sinA) su) `add` (scale ((sin phi) * sinA) sv) `add` (scale cosA sw)

--get2Rands :: StdGen -> (Double, Double, StdGen)
--get2Rands rand = (fst r1, fst r2, snd r2)
--  where
--      r1 = randomR (0,1) rand
--      r2 = randomR (0,1) $ snd r1

--calculateShadow :: Ray -> [Object] -> Object -> Colour
--calculateShadow ray@(Ray o _) scene light
--  | closestObIsLight = Vector3 1 1 1
--  | otherwise = Vector3 (0) (0) (0)
--  where
--      dist = mag $ (centerOb light) `sub` o
--      hits = catMaybes $ map (intersectOb ray) scene
--      closest = filter (\(d, _) -> d <= dist)
--      closestOb = closestIntersectOb ray scene
--      getOb (Just (_,_,o)) = o
--      closestObIsLight = if isNothing closestOb then True else (if (getOb closestOb) == light then True else False)
