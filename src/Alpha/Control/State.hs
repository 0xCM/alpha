module Alpha.Control.State where

import Data.STRef
import Control.Monad(forever, (>>=))
import Control.Monad.ST
import Control.Concurrent(forkFinally)
import System.IO

import Network.Socket



-- | Creates a mutable reference cell in the current state thread
pack:: a -> ST s (STRef s a)    
pack = newSTRef

-- | Dereferences the cell to obtain a value
unpack::STRef s a -> ST s a
unpack = readSTRef

-- | Replace the existing value with a new value
replace::STRef s a -> a -> ST s ()
replace = writeSTRef


