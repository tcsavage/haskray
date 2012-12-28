module Main where

import HaskRay

import Criterion.Main

import System.Random

objects :: [Object]
objects = [Object (Plane (normalize (Vector3 0 (-1) 0)) 5) (Diffuse (Vector3 0.8 0.8 0.8))
        ,Object (Plane (normalize (Vector3 0 (1) 0)) 18) (Diffuse (Vector3 0.8 0.8 0.8))
        ,Object (Plane (normalize (Vector3 1 0 0)) (14)) (Diffuse (Vector3 0.8 0 0))
        ,Object (Plane (normalize (Vector3 (-1) 0 0)) (14)) (Diffuse (Vector3 0 0.8 0))
        ,Object (Plane (normalize (Vector3 0 0 (-1))) (18)) (Diffuse (Vector3 0.8 0.8 0.8))
        ,Object (Sphere (Vector3 1 1 1) 3) (Transmissive 1.05 0.9)
        ,Object (Sphere (Vector3 5 1 10) 4) (Reflective)
        ,Object (Sphere (Vector3 (-8) 0 8) 5) (Diffuse (Vector3 0 1 0))
        ,Object (Sphere (Vector3 8 3 4) 2) (Diffuse (Vector3 1 1 0))
        ,Object (Sphere (Vector3 (-8) (-15) 2) 1) (Emissive (Vector3 1 0 0) 400)
        --,Object axes (Diffuse (Vector3 1 0.5 0))
        ]

camera :: View
camera = View
    {position = (Vector3 0 (-10) (-80))
    ,lookAt = (Vector3 0 (-5) 10)
    ,upVec = (Vector3 0 (-1) 0)
    ,fov = 30
    }

scene :: Scene
scene = Scene objects camera

main :: IO ()
main = do
    randomSeed <- newStdGen
    let settings = Settings 853 480 5 randomSeed
    defaultMain
        [bgroup "render"
            [bench "render" $ whnf (\s -> render settings s) scene
            ,bench "render (unopt)" $ whnf (\s -> render settings s) scene
            ]
        ]
