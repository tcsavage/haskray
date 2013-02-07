module Main where

import HaskRay

import Data.Binary
import System.Environment
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [inp, out] <- getArgs
    putStr "Loading file..."
    traced <- decodeFile inp
    putStr " Done\nEvaluating image..."
    saveBMP' (eval traced) out
    putStrLn " Finished."
