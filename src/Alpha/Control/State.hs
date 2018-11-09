module Alpha.Control.State where

import Data.STRef
import Control.Monad(forever, (>>=))
import Control.Monad.ST
import Control.Concurrent(forkFinally)
import System.IO
import Network.Socket

import Alpha.Canonical

-- instance (b ~ ST s (STRef s a)) => Packable a b where
--     pack = newSTRef
    
    

-- | Creates a mutable reference cell in the current state thread
spack:: a -> ST s (STRef s a)    
spack = newSTRef

-- | Dereferences the cell to obtain a value
sunpack::STRef s a -> ST s a
sunpack = readSTRef

-- | Replace the existing value with a new value
replace::STRef s a -> a -> ST s ()
replace = writeSTRef


