{-# LANGUAGE CPP, OverloadedStrings, Arrows #-}

module Main (main) where

import HaskRay
import System.Environment
import System.Random.Mersenne.Pure64
import Data.Char
import Data.Maybe
import Control.Arrow
import Data.Time.Clock

import Settings

-- Exploring a serialisable DSL for materials and scenes.
data MaterialDSL = Holdout
                 | Diffuse !Colour
                 | Emissive
                 deriving (Show, Read, Eq)

translateDSL :: MaterialDSL -> Material () (BSDF Colour)
translateDSL Holdout = arr $ \ () -> holdout
translateDSL (Diffuse col) = diffuseM col
translateDSL Emissive = emissiveM

diffuseM :: Colour -> Material () (BSDF Colour)
diffuseM col = proc () -> do
    diffuse -< col

emissiveM :: Material () (BSDF Colour)
emissiveM = proc () -> do
    out <- emissive -< (Vector3 1 1 1, 100)
    returnA -< out

objects :: Texture -> [Shape]
objects tex = [mkPlaneShape (Plane (normalize (Vector3 0 (-1) 0)) 5) (diffuseM (Vector3 0.8 0.8 0.8))
        ,mkPlaneShape (Plane (normalize (Vector3 0 (1) 0)) 18) (diffuseM (Vector3 0.8 0.8 0.8))
        ,mkPlaneShape (Plane (normalize (Vector3 1 0 0)) (14)) (diffuseM (Vector3 0.8 0 0))
        ,mkPlaneShape (Plane (normalize (Vector3 (-1) 0 0)) (14)) (diffuseM (Vector3 0 0.8 0))
        ,mkPlaneShape (Plane (normalize (Vector3 0 0 (-1))) (18)) (diffuseM (Vector3 0.8 0.8 0.8))
        --,mkSphereShape (Sphere (Vector3 1 1 1) 3) (Transmissive 1.05 0.9)
        --,mkSphereShape (Sphere (Vector3 5 1 10) 4) mirror
        ,mkSphereShape (Sphere (Vector3 (-8) 0 8) 5) (diffuseM (Vector3 0 1 0))
        --,mkSphereShape (Sphere (Vector3 (-8) 0 8) 5) (Shaded $ Textured tex)
        ,mkSphereShape (Sphere (Vector3 8 3 4) 2) (diffuseM (Vector3 1 0 0))
        ,mkSphereShape (Sphere (Vector3 (2) (-15) (-8)) 1) emissiveM
        --,mkSphereShape (Sphere (Vector3 (-8) (-15) (0)) 0.5) emissiveM
        ]

camera :: View
camera = View
    {position = (Vector3 0 (-10) (-80))
    ,lookAt = (Vector3 0 (-5) 10)
    ,upVec = (Vector3 0 (-1) 0)
    ,fov = 30
    }

readArgs :: [String] -> [Setting]
readArgs [] = []
readArgs ("-w":w:xs)
    | all isDigit w = Width (read w) : readArgs xs
    | otherwise = error $ "Width not a number"
readArgs ("-h":h:xs)
    | all isDigit h = Height (read h) : readArgs xs
    | otherwise = error $ "Height not a number"
readArgs ("-s":s:xs)
    | all isDigit s = Samples (read s) : readArgs xs
    | otherwise = error $ "Samples not a number"
readArgs ("-r":r:xs) = (RandomGen $ pureMT $ read r) : readArgs xs
readArgs ("-g":m:xs) = (GIMode $ read m) : readArgs xs
readArgs ("-i":fp:xs) = InputFile fp : readArgs xs
readArgs ("-o":fp:xs) = OutputFile fp : readArgs xs
readArgs args = error ("Unrecognised arguments: " ++ show args)

time :: IO () -> IO ()
time action = do
    t1 <- getCurrentTime
    action
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1

main :: IO ()
main = do
    opts <- getArgs
    randomSeed <- newPureMT
    let settings = readArgs opts
    let rsettings = fromJust $ fromSettingList randomSeed $ readArgs opts
    tex <- loadTexture "tex-spheremap.bmp"
    let scene = Scene (objects tex) camera
    case getFilePath settings of
        Just filepath -> do
            putStrLn $ "Rendering (seed: " ++ (show randomSeed) ++ ")..."
            time $ do
                let pbuf = render rsettings scene
                saveBMP pbuf filepath
        otherwise -> error "No output file given"
