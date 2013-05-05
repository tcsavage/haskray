module Settings where

import System.Random
import Control.Applicative
import HaskRay

data Setting = Width Int
             | Height Int
             | Samples Int
             | RandomGen StdGen
             | GIMode Int
             | InputFile FilePath
             | OutputFile FilePath
             deriving (Show)

getWidth :: [Setting] -> Maybe Int
getWidth [] = Nothing
getWidth ((Width x):_) = Just x
getWidth (_:xs) = getWidth xs

getHeight :: [Setting] -> Maybe Int
getHeight [] = Nothing
getHeight ((Height x):_) = Just x
getHeight (_:xs) = getHeight xs

getSamples :: [Setting] -> Maybe Int
getSamples [] = Nothing
getSamples ((Samples x):_) = Just x
getSamples (_:xs) = getSamples xs

getSeed :: [Setting] -> Maybe StdGen
getSeed [] = Nothing
getSeed ((RandomGen x):_) = Just x
getSeed (_:xs) = getSeed xs

getGIMode :: [Setting] -> Maybe Int
getGIMode [] = Nothing
getGIMode ((GIMode x):_) = if x > 0 then Just x else Nothing
getGIMode (_:xs) = getGIMode xs

getInputFile :: [Setting] -> Maybe FilePath
getInputFile [] = Nothing
getInputFile ((InputFile x):_) = Just x
getInputFile (_:xs) = getInputFile xs

getFilePath :: [Setting] -> Maybe FilePath
getFilePath [] = Nothing
getFilePath ((OutputFile x):_) = Just x
getFilePath (_:xs) = getFilePath xs

-- | Generate a renderer settings value from a list of settings.
fromSettingList :: StdGen -> [Setting] -> Maybe Settings
fromSettingList defRand xs = do
    w <- getWidth xs
    h <- getHeight xs
    s <- getSamples xs
    r <- (getSeed xs <|> Just defRand)
    let g = getGIMode xs
    return $ Settings w h s r g
