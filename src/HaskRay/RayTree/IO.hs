{-# LANGUAGE ScopedTypeVariables #-}

module HaskRay.RayTree.IO () where

import HaskRay.RayTree.Light
import HaskRay.RayTree.Sample
import HaskRay.Vector

import Control.Monad
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as R ()
import qualified Data.Array.Repa.Eval as R
import Data.Binary

newtype AsShape s = AsShape s

instance (R.Shape s) => Binary (AsShape s) where
    put (AsShape s) = put $ R.listOfShape s
    get = get >>= (return . AsShape . R.shapeOfList)

instance (Binary e, R.Shape sh, R.Source r e, R.Target r e) => Binary (R.Array r sh e) where
    put arr = (put . AsShape $ R.extent arr) >> (put $ R.toList arr)
    get = do
        AsShape shape <- get
        elements <- get
        return (R.fromList shape elements)

instance Binary a => Binary (Vector3 a) where
    put (Vector3 x y z) = do
        put x
        put y
        put z
    get = do
        x <- get
        y <- get
        z <- get
        return $ Vector3 x y z

instance Binary a => Binary (Vector2 a) where
    put (Vector2 x y) = do
        put x
        put y
    get = do
        x <- get
        y <- get
        return $ Vector2 x y

instance Binary Sample where
    put Background = put (0 :: Word8)
    put (Diff col shadows gi) = do
        put (1 :: Word8)
        put col
        put (length shadows)
        mapM_ put shadows
        put gi
    put (Emm col) = do
        put (2 :: Word8)
        put col
    put (Reflection sample) = do
        put (3 :: Word8)
        put sample
    put (Refraction refraction reflection mix) = do
        put (4 :: Word8)
        put refraction
        put reflection
        put mix
    put Dead = put (5 :: Word8)
    get = do
        (t :: Word8) <- get
        case t of
            0 -> return Background
            1 -> do
                col <- get
                lenShadows <- get
                shadows <- replicateM lenShadows get
                gi <- get
                return $ Diff col shadows gi
            2 -> do
                col <- get
                return $ Emm col
            3 -> do
                reflection <- get
                return $ Reflection reflection
            4 -> do
                refraction <- get
                reflection <- get
                mix <- get
                return $ Refraction refraction reflection mix
            5 -> return Dead

instance Binary Shadow where
    put (Shadow col) = put col
    get = do
        col <- get
        return $ Shadow col
