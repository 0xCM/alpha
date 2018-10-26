{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Data.Vector
(
    Vector, unsized, Vectored(..)
)
where
import qualified Data.Vector as V
import Alpha.Base
import Alpha.Data.Tuple
import Alpha.Canonical
import Alpha.Data.Numbers
import qualified Data.List as List

newtype Vector (n::Nat) a = Vector (V.Vector a)
    deriving (Show,Generic,Data,Typeable)

class Vectored (n::Nat) a b where    
    vector::a -> Vector n b

unsized::Vector n a -> V.Vector a
unsized = coerce

instance Wrapped (Vector n a) (V.Vector a) where
    unwrap = coerce

instance Length (V.Vector a) where
    length = convert . V.length

instance Indexed (Vector n a) a where
    item (Vector a) i = a V.! i

-- instance Tupled 2 (Vector 2 e) where    
--     type Tuple' (Vector 2 e) = Tuple (e, e)
--     tuple  x = ( x ! 0, x ! 1) 

instance Vectored 2 (a,a) a where
    vector (x1, x2) = Vector( V.fromList [x1, x2])
    
-- instance Tupled 3 (Vector 3 e) where    
--     type Tuple' (Vector 3 e) = Tuple (e, e, e)
--     tuple  x = ( x ! 0, x ! 1, x ! 2) 

instance Vectored 3 (a, a, a) a where
    vector (x1, x2, x3) = Vector( V.fromList [x1, x2, x3])
        
-- instance Tupled 4 (Vector 4 e) where    
--     type Tuple' (Vector 4 e) = Tuple (e, e, e, e)
--     tuple  x = ( x ! 0, x ! 1, x ! 2, x ! 3) 

instance Vectored 4 (a, a, a, a) a where
    vector (x1, x2, x3, x4) = Vector( V.fromList [x1, x2, x3, x4])
    
-- instance Tupled 5 (Vector 5 e) where    
--     type Tuple' (Vector 5 e) = Tuple (e, e, e, e, e)
--     tuple  x = ( x ! 0, x ! 1, x ! 2, x ! 3, x ! 4) 

instance Vectored 4 (a, a, a, a, a) a where
    vector (x1, x2, x3, x4, x5) = Vector( V.fromList [x1, x2, x3, x4, x5])
    