{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module HaskRay.Monad
(Render
,evalRender
,runRender
,getRandomR
,liftRand
,module Control.Monad.Trans.State
,module System.Random
,module System.Random.Mersenne.Pure64
) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import System.Random.Mersenne.Pure64
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies

-- | Render monad type alias.
type Render a = State PureMT a

instance NFData (Render a) where

-- | Run Render monad.
evalRender :: Render a -> PureMT -> a
evalRender = evalState

runRender :: Render a -> PureMT -> (a, PureMT)
runRender = runState

-- | Internal utility function for lifting functions from 'System.Random' into 'Render'.
liftRand :: (PureMT -> (a, PureMT)) -> Render a
liftRand f = do
    rand <- get
    let (result, rand') = f rand
    put rand'
    return result

-- | Random actions.
getRandomR :: (Random a) => (a, a) -> Render a
getRandomR = liftRand . randomR
