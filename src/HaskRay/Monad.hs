{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module HaskRay.Monad
(RState(..)
,Render
,getRand
,setRand
,getDepth
,setDepth
,evalRender
,runRender
,getRandomR
,liftRand
,module Control.Monad.Trans.State
,module System.Random
,module System.Random.Mersenne.Pure64
) where

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import System.Random.Mersenne.Pure64
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies

data RState = RState { rand :: !PureMT, depth :: !Int }

-- | Render monad type alias.
type Render a = State RState a

instance NFData (Render a) where

getRand :: Render PureMT
getRand = rand <$> get

setRand :: PureMT -> Render ()
setRand rand' = modify $ \state -> state { rand = rand' }

getDepth :: Render Int
getDepth = depth <$> get

setDepth :: Int -> Render ()
setDepth depth' = modify $ \state -> state { depth = depth' }

-- | Run Render monad.
evalRender :: Render a -> RState -> a
evalRender = evalState

runRender :: Render a -> RState -> (a, RState)
runRender = runState

-- | Internal utility function for lifting functions from 'System.Random' into 'Render'.
liftRand :: (PureMT -> (a, PureMT)) -> Render a
liftRand f = do
    r <- getRand
    let (result, r') = f r
    setRand r'
    return result

-- | Random actions.
getRandomR :: (Random a) => (a, a) -> Render a
getRandomR = liftRand . randomR
