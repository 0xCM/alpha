{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Data.Vector
(
    unsized, 
    vector    
)
where
import qualified Data.Vector as V
import Alpha.Base
import Alpha.Data.Product
import Alpha.Canonical
import Alpha.Data.Numbers
import qualified Data.List as List

unsized::Vector n a -> V.Vector a
unsized (Col v) = v
unsized (Row v) = v

transpose::Vector n a -> Vector n a
transpose  (Col v) = Row v
transpose  (Row v) = Col v

-- Creates a sized vector from a list    
vector::[a] -> Vector n a
vector src = Col(V.fromList src)

instance Length (Vector n a) where
    length = convert . V.length . unsized

instance Indexed (Vector n a) a where
    item (Col a) i = a V.! i
    item (Row a) i = a V.! i

instance Vectored 2 (a,a) a where
    col (x1, x2) = Col $ V.fromList [x1, x2]

instance Covectored 2 (a,a) a where
    row (x1, x2) = Row $ V.fromList [x1, x2]
        
instance Vectored 3 (a, a, a) a where
    col (x1, x2, x3) = Col $ V.fromList [x1, x2, x3]

instance Covectored 3 (a,a,a) a where    
    row (x1, x2, x3) = Row $ V.fromList [x1, x2, x3]

instance Vectored 4 (a, a, a, a) a where
    col (x1, x2, x3, x4) = Col $ V.fromList [x1, x2, x3, x4]
    
instance Covectored 4 (a, a, a, a) a where    
    row (x1, x2, x3, x4) = Row $ V.fromList [x1, x2, x3, x4] 

instance Vectored 5 (a, a, a, a, a) a where
    col (x1, x2, x3, x4, x5) = Col $ V.fromList [x1, x2, x3, x4, x5]

instance Covectored 5 (a, a, a, a, a) a where    
    row (x1, x2, x3, x4, x5) = Row $ V.fromList [x1, x2, x3, x4, x5]        
