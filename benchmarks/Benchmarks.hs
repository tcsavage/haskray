module Main where

import HaskRay

import Criterion.Main

import System.Random

writeFun :: (Scene -> PixBuf) -> Scene -> IO ()
writeFun f s = do
    let pb = f s
    savePpm "bench-testfile.ppm" pb

mesh3 = (Mesh (Vector3 0 (-5) (-5)) [(Triangle (Vertex (Vector3 (-2.165064) (1.113091) (1.800000)) (Vector3 (-0.695334) (0.553667) (0.458174)) (Vector2 0 0)) (Vertex (Vector3 (-0.000000) (-0.136910) (-8.200000)) (Vector3 (0.000000) (0.025117) (-0.999664)) (Vector2 0 0)) (Vertex (Vector3 (-0.000000) (-2.636908) (1.800001)) (Vector3 (0.000000) (-0.884854) (0.465835)) (Vector2 0 0))), (Triangle (Vertex (Vector3 (-0.000000) (-2.636908) (1.800001)) (Vector3 (0.000000) (-0.884854) (0.465835)) (Vector2 0 0)) (Vertex (Vector3 (-0.000000) (-0.136910) (-8.200000)) (Vector3 (0.000000) (0.025117) (-0.999664)) (Vector2 0 0)) (Vertex (Vector3 (2.165063) (1.113092) (1.800000)) (Vector3 (0.695334) (0.553667) (0.458174)) (Vector2 0 0))), (Triangle (Vertex (Vector3 (-0.000000) (0.352051) (1.800000)) (Vector3 (0.000000) (0.631611) (0.775262)) (Vector2 0 0)) (Vertex (Vector3 (-0.000000) (-0.136910) (-8.200000)) (Vector3 (0.000000) (0.025117) (-0.999664)) (Vector2 0 0)) (Vertex (Vector3 (-2.165064) (1.113091) (1.800000)) (Vector3 (-0.695334) (0.553667) (0.458174)) (Vector2 0 0))), (Triangle (Vertex (Vector3 (-0.000000) (-2.636908) (1.800001)) (Vector3 (0.000000) (-0.884854) (0.465835)) (Vector2 0 0)) (Vertex (Vector3 (-0.000000) (0.352051) (1.800000)) (Vector3 (0.000000) (0.631611) (0.775262)) (Vector2 0 0)) (Vertex (Vector3 (-2.165064) (1.113091) (1.800000)) (Vector3 (-0.695334) (0.553667) (0.458174)) (Vector2 0 0))), (Triangle (Vertex (Vector3 (-0.000000) (-0.136910) (-8.200000)) (Vector3 (0.000000) (0.025117) (-0.999664)) (Vector2 0 0)) (Vertex (Vector3 (-0.000000) (0.352051) (1.800000)) (Vector3 (0.000000) (0.631611) (0.775262)) (Vector2 0 0)) (Vertex (Vector3 (2.165063) (1.113092) (1.800000)) (Vector3 (0.695334) (0.553667) (0.458174)) (Vector2 0 0))), (Triangle (Vertex (Vector3 (-0.000000) (0.352051) (1.800000)) (Vector3 (0.000000) (0.631611) (0.775262)) (Vector2 0 0)) (Vertex (Vector3 (-0.000000) (-2.636908) (1.800001)) (Vector3 (0.000000) (-0.884854) (0.465835)) (Vector2 0 0)) (Vertex (Vector3 (2.165063) (1.113092) (1.800000)) (Vector3 (0.695334) (0.553667) (0.458174)) (Vector2 0 0)))])

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
        ,Object mesh3 (Diffuse (Vector3 1 0.5 0))
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
            ,bench "render (unopt)" $ whnf (\s -> renderUnOpt settings s) scene
            ]
        --,bgroup "render & save"
        --    [bench "render" $ whnfIO $ (\s -> writeFun (render settings) s) scene
        --    ,bench "render (unopt)" $ whnfIO $ (\s -> writeFun (renderUnOpt settings) s) scene
        --    ]
        ]
