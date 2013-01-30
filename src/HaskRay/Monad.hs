{-# LANGUAGE FlexibleInstances #-}

module HaskRay.Monad
{-# WARNING "The Rand StdGen instance of MonadParallel violates the expectation that bindM2 is equivelant to \\f ma mb-> do {a <- ma; b <- mb; f a b}" #-}
(Render
,runRender
,module Control.Monad.Reader
,module Control.Monad.Random
) where

import HaskRay.Octree

import Control.Monad (replicateM)
import Control.Monad.Trans (lift)
import Control.Monad.Reader
import Control.Monad.Random
import Control.Monad.Parallel
import Control.Parallel
import System.Random (getStdGen, StdGen)

import Debug.Trace

-- | Render monad type alias.
type Render a = ReaderT ObjectStructure (Rand StdGen) a

-- | Doesn't produce same result as @ma >>= (\a -> mb >>= (\b -> f a b))@
instance MonadParallel (Rand StdGen) where
    bindM2 f ma mb = do
        split1 <- getSplit
        split2 <- getSplit
        let a = evalRand ma split1
        let b = evalRand mb split2
        a `par` b `pseq` f a b

-- | Run Render monad.
runRender :: Render a -> ObjectStructure -> StdGen -> a
runRender m a b = traceEvent "runRender" $ evalRand (runReaderT m a) b
