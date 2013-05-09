{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

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
import Control.Monad.Parallel
import Control.Monad.State
import System.Random
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies

-- | Render monad type alias.
type Render a = State StdGen a

instance NFData (Render a) where

-- | Doesn't produce same result as @ma >>= (\a -> mb >>= (\b -> f a b))@.
instance MonadParallel (State StdGen) where
    bindM2 f ma mb = do
        (split1, split2) <- fmap split get
        (ret, rand') <- return $ runEval $ do
            (a, rand1) <- rpar $ runRender ma split1
            (b, rand2) <- rseq $ runRender mb split2
            x <- rseq $ f a b
            return (x, rand1)
        put rand'
        ret

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
