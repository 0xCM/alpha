-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Stateful where

import Data.STRef
import Control.Monad(forever, (>>=))
import Control.Monad.ST
import Control.Concurrent(forkFinally)
import System.IO
import Network.Socket

import Alpha.Canonical.Common.Root

-- | Creates a mutable reference cell in the current state thread
spack:: a -> ST s (STRef s a)    
spack = newSTRef

-- | Dereferences the cell to obtain a value
sunpack::STRef s a -> ST s a
sunpack = readSTRef

-- | Replace the existing value with a new value
replace::STRef s a -> a -> ST s ()
replace = writeSTRef

newtype RefCell a = RefCell (IORef a)
    deriving (Eq)

class StateCell a where
    newCell::a -> IO (RefCell a)
    newCell a = do
        c <- newIORef a
        pure (RefCell c)
        
    readCell::RefCell a -> IO a
    readCell (RefCell x) = readIORef x

    writeCell::RefCell a -> a -> IO()
    writeCell (RefCell x) y = writeIORef x y

    updateCell::RefCell a -> (a -> (a,b)) -> IO b
    updateCell (RefCell x) f = atomicModifyIORef' x f            


instance (Show a) => Show (RefCell a) where
    show = show . shredIO . readCell
    
instance StateCell a

