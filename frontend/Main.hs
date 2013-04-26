{-# LANGUAGE CPP, OverloadedStrings #-}

module Main where

import HaskRay
import System.Environment
import System.Random
import Data.Char
import Data.Maybe
import System.IO
import Control.Monad
import Text.Printf
import Data.Binary

import Settings

#ifdef GLView
import Control.Concurrent.Async
import GLDisplay
#endif

mesh = Mesh (Vector3 8 (-5) (-5)) [Triangle v1 v2 v3]
    where
        v1 = Vertex (Vector3 0 0 0) (Vector3 0 0 0) (Vector2 0 0)
        v2 = Vertex (Vector3 3 0 0) (Vector3 0 0 0) (Vector2 0 0)
        v3 = Vertex (Vector3 0 3 0) (Vector3 0 0 0) (Vector2 0 0)

objects :: Texture -> [Object]
objects tex = [Object (Plane (normalize (Vector3 0 (-1) 0)) 5) (Shaded $ Flat (Vector3 0.8 0.8 0.8))
        ,Object (Plane (normalize (Vector3 0 (1) 0)) 18) (Shaded $ Flat (Vector3 0.8 0.8 0.8))
        ,Object (Plane (normalize (Vector3 1 0 0)) (14)) (Shaded $ Flat (Vector3 0.8 0 0))
        ,Object (Plane (normalize (Vector3 (-1) 0 0)) (14)) (Shaded $ Flat (Vector3 0 0.8 0))
        ,Object (Plane (normalize (Vector3 0 0 (-1))) (18)) (Shaded $ Flat (Vector3 0.8 0.8 0.8))
        ,Object (Sphere (Vector3 1 1 1) 3) (Transmissive 1.05 0.9)
        ,Object (Sphere (Vector3 5 1 10) 4) (Reflective)
        --,Object (Sphere (Vector3 (-8) 0 8) 5) (Shaded $ Flat (Vector3 0 1 0))
        ,Object (Sphere (Vector3 (-8) 0 8) 5) (Shaded $ Textured tex)
        ,Object (Sphere (Vector3 8 3 4) 2) (Shaded $ Flat (Vector3 1 0 0))
        --,Object (Sphere (Vector3 2 (-15) 2) 1) (Emissive (Vector3 1 0 0) 400)
        ,Object (Sphere (Vector3 (2) (-15) (-8)) 1) (Emissive (Vector3 1 1 1) 400)
        --,Object mesh3 (Shaded $ Flat (Vector3 1 0.5 0))
        --,Object texTri (Shaded $ Textured tex)
        ]

camera :: View
camera = View
    {position = (Vector3 0 (-10) (-80))
    ,lookAt = (Vector3 0 (-5) 10)
    ,upVec = (Vector3 0 (-1) 0)
    ,fov = 80
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
readArgs ("-r":r:xs) = (RandomGen $ read r) : readArgs xs
readArgs ("-g":m:xs) = (GIMode $ read m) : readArgs xs
readArgs ("-i":fp:xs) = InputFile fp : readArgs xs
readArgs ("-o":fp:xs) = OutputFile fp : readArgs xs
#ifdef GLView
readArgs ("-glview":xs) = OpenGLView : readArgs xs
#else
readArgs ("-glview":_) = error "Not compiled with glview enabled"
#endif
readArgs args = error ("Unrecognised arguments: " ++ show args)

main :: IO ()
main = do
    opts <- getArgs
    randomSeed <- newStdGen
    let settings = readArgs opts
    let rsettings = fromJust $ fromSettingList randomSeed $ readArgs opts
    scenestr <- getContents
    tex <- loadTexture "tex-spheremap.bmp"
    let inp = fromMaybe (error "No input file given") $ getInputFile settings
    sceneText <- readFile inp
    scene <- fmap optimiseScene $ deserialize sceneText
    let pbuf = render rsettings scene
    case getFilePath settings of
        Just filepath -> do
            putStrLn $ "Rendering (seed: " ++ (show randomSeed) ++ ")..."
#ifdef GLView
            when (getOpenGLView settings) $ glDisp rsettings scene pbuf
#endif
            saveBMP pbuf filepath
        otherwise -> error "No output file given"
