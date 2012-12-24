module GLDisplay.CameraMode where

data CameraMode = CameraMode { usescene :: Bool, xrot :: Double, yrot :: Double } deriving (Show, Eq)
