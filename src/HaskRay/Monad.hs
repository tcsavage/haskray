{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module HaskRay.Monad
(RState(..)
,Render
,getRand
,withReducedDepth
,evalRender
,runRender
,getRandomR
,liftRand
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

-- | Get the raw random generator.
getRand :: Render PureMT
getRand = rand <$> get

-- | Set the random generator. Internal only.
setRand :: PureMT -> Render ()
setRand rand' = modify $ \state -> state { rand = rand' }

-- Internal.
getDepth :: Render Int
getDepth = depth <$> get

-- Internal.
setDepth :: Int -> Render ()
setDepth depth' = modify $ \state -> state { depth = depth' }

-- | Perform an action one level down in depth - with a default if already at max depth.
withReducedDepth :: Render a -- ^ Default action
                 -> Render a -- ^ Action to perform at d-1
                 -> Render a
withReducedDepth def action = do
	d <- getDepth
	case d > 0 of
		True -> do
			setDepth (d-1)
			out <- action
			setDepth d
			return out
		False -> def

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
getRandomR :: (Random a) => (a, a) -- ^ Range of result
                         -> Render a
getRandomR = liftRand . randomR
