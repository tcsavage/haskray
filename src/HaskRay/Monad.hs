module HaskRay.Monad
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
import System.Random (getStdGen, StdGen)

-- | Render monad type alias.
type Render a = ReaderT ObjectStructure (Rand StdGen) a

-- | Run Render monad.
runRender :: Render a -> ObjectStructure -> StdGen -> a
runRender m a b = evalRand (runReaderT m a) b
