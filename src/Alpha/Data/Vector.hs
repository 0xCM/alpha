{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Data.Vector where
import qualified Data.Vector as V
import Data.Function
import Alpha.Base
import Alpha.Data.Tuple
import Alpha.Canonical
import Alpha.Data.Numbers
import qualified Data.List as List

newtype Vector (n::Nat) a = Vector (V.Vector a)
    deriving (Show)

v2::T2 e -> Vector 2 e
v2 (x1, x2) = Vector  (V.fromList [x1, x2])    
    
v3::T3 e -> Vector 3 e
v3 (x1, x2, x3) = Vector  (V.fromList [x1, x2, x3])    

v4::T4 e -> Vector 4 e
v4 (x1, x2, x3, x4) = Vector  (V.fromList [x1, x2, x3, x4])

v5::T5 e -> Vector 5 e
v5 (x1, x2, x3, x4, x5) = Vector  (V.fromList [x1, x2, x3, x4, x5])

instance Length (V.Vector a) where
    length = convert . V.length

instance Indexed (Vector n a) a where
    item (Vector a) i = a V.! i

instance Tupled (Vector 2 e) where    
    type Tuple' (Vector 2 e) = T2 e
    tuple  x = ( x ! 0, x ! 1) 

instance Tupled (Vector 3 e) where    
    type Tuple' (Vector 3 e) = T3 e
    tuple  x = ( x ! 0, x ! 1, x ! 2) 
            
instance Tupled (Vector 4 e) where    
    type Tuple' (Vector 4 e) = T4 e
    tuple  x = ( x ! 0, x ! 1, x ! 2, x ! 3) 

instance Tupled (Vector 5 e) where    
    type Tuple' (Vector 5 e) = T5 e
    tuple  x = ( x ! 0, x ! 1, x ! 2, x ! 3, x ! 4) 
            
    
vector::Tn e -> V.Vector e
vector (T2 (x1,x2)) = V.fromList [x1 , x2]
vector (T3 (x1,x2,x3)) = V.fromList [x1 , x2, x3]
vector (T4 (x1,x2,x3,x4)) = V.fromList [x1 , x2, x3, x4]
vector (T5 (x1,x2,x3,x4,x5)) = V.fromList [x1 , x2, x3, x4,x5]