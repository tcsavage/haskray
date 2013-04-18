module HaskRay.RayTree.Light
(
Shadow(..),
traceLight
) where

import HaskRay.Geometry
import HaskRay.Material
import HaskRay.Monad
import HaskRay.Octree
import HaskRay.Vector

import Data.Maybe
import Data.Typeable

-- | Shadow ray retult.
data Shadow = Shadow !Colour deriving (Show, Eq)

-- | Calculate per-light shading.
traceLight :: Intersection -> Colour -> Object -> Render Shadow
traceLight (Intersection norm point _ _) colour light = do
    fact <- doLighting point norm light
    return $ Shadow $ scale fact colour
traceLight _ _ _ = error "HaskRay.RayTree.Light.traceLight: Trying to light non-diffuse material."

-- TODO: Shadow through reflection/refraction
doLighting :: Vec3 -> Vec3 -> Object -> Render Scalar
doLighting x norm light@(Object ls (Emissive colour empow)) = do
    (obs, _) <- ask
    eps1 <- getRandomR (0, 1)
    eps2 <- getRandomR (0, 1)
    let sphere = fromMaybe (error "HaskRay.RayTree.Light.doLighting: Emissive surface not sphere.") $ cast ls
    let (Sphere center radius) = sphere
    let sw = center `sub` x
    let su = normalize (if abs (x3 sw) > 1 then Vector3 0 1 0 else Vector3 1 0 0)
    let sv = sw `cross` su
    let cosAMax = sqrt (1 - radius * radius / ((x `sub` center) `dot` (x `sub` center)))
    let cosA = 1 - eps1 + (eps1 * cosAMax)
    let sinA = sqrt $ 1 - cosA * cosA
    let phi = 2 * pi * eps2
    let l = normalize $ scale (cos phi * sinA) su `add` scale (sin phi * sinA) sv `add` scale cosA sw
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
        isReflectiveOrTransmissive (_, Intersection _ _ _ Reflective, _) = True
        isReflectiveOrTransmissive (_, Intersection _ _ _ (Transmissive _ _), _) = True
        isReflectiveOrTransmissive _ = False
doLighting _ _ _ = error "HaskRay.RayTree.Light.doLighting: Trying to light with non-emissive material."
