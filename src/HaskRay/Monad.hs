module HaskRay.Monad
(Render
,evalRender
,runRender
,getRandomR
,liftRand
,module Control.Monad.State
,module System.Random
) where

import Control.Monad (replicateM)
import Control.Monad.State
import System.Random

-- | Render monad type alias.
type Render a = State StdGen a

-- | Run Render monad.
evalRender :: Render a -> StdGen -> a
evalRender = evalState

runRender :: Render a -> StdGen -> (a, StdGen)
runRender = runState

-- | Internal utility function for lifting functions from 'System.Random' into 'Render'.
liftRand :: (StdGen -> (a, StdGen)) -> Render a
liftRand f = do
    rand <- get
    let (result, rand') = f rand
    put rand'
    return result

-- | Random actions.
getRandomR :: (Random a) => (a, a) -> Render a
getRandomR = liftRand . randomR
