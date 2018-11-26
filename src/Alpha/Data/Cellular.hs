module Alpha.Data.Cellular where

import Alpha.Base
import Alpha.Canonical
import Data.IORef

newtype RefCell a = RefCell (IORef a)

class Cellular a where
    createcell::a -> RefCell a
    createcell = RefCell . shredIO . newIORef

    readcell::RefCell a -> a
    readcell (RefCell x) = shredIO $ readIORef x

    writecell::RefCell a -> a -> ()
    writecell (RefCell x) y = shredIO $ writeIORef x y

    updatecell::RefCell a -> (a -> (a,b)) -> b
    updatecell (RefCell x) f = shredIO $ atomicModifyIORef' x f
    