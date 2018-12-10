module Alpha.System.Cellular
(
    RefCell(..),
    Cellular(..)

) where
import Alpha.Base

newtype RefCell a = RefCell (IORef a)
    deriving (Eq)

class Cellular a where
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
    
instance Cellular a

