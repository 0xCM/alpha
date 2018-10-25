{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Alpha.Data.Tuple
(
    Tuple, Tupled(..)   


)
where
import Alpha.Base
import Alpha.Canonical
import qualified Data.Vector as V

type family Tuple a        
type instance Tuple (x1, x2) = (x1, x2)
type instance Tuple (x1, x2, x3) = (x1, x2, x3)
type instance Tuple (x1, x2, x3, x4) = (x1, x2, x3, x4)
type instance Tuple (x1, x2, x3, x4, x5)  = (x1 ,x2, x3, x4, x5)
type instance Tuple (x1, x2, x3, x4, x5, x6)  = (x1 ,x2, x3, x4, x5, x6)
type instance Tuple (x1, x2, x3, x4, x5, x6, x7)  = (x1 ,x2, x3, x4, x5, x6, x7)
type instance Tuple (x1, x2, x3, x4, x5, x6, x7, x8)  = (x1 ,x2, x3, x4, x5, x6, x7, x8)

class Tupled (n::Nat) a where 
    type Tuple' a       
    type Tuple' a = Tuple a
    tuple::a -> Tuple' a


-- instance (Semigroup x1, Semigroup x2) =>  Semigroup (Tuple (x1, x2)) where
--     (<>) (a1, b1) (a2, b2) = (a1 <> a2, b1 <> b2)

instance Tupled 2 (x1, x2) where    
    tuple (x1, x2) = (x1, x2)
    
instance Tupled 3 (x1, x2, x3) where
    tuple (x1, x2, x3) = (x1, x2, x3)
    
instance Tupled 4 (x1, x2, x3, x4) where
    tuple (x1, x2, x3, x4) = (x1, x2, x3, x4)
    
instance Tupled 5 (x1, x2, x3, x4, x5) where
    tuple (x1, x2, x3, x4, x5) = (x1, x2, x3, x4, x5)
    
instance Tupled 6 (x1, x2, x3, x4, x5, x6) where
    tuple (x1, x2, x3, x4, x5, x6) = (x1, x2, x3, x4, x5, x6)    
            
instance Tupled 7 (x1, x2, x3, x4, x5, x6, x7) where
    tuple (x1, x2, x3, x4, x5, x6, x7) = (x1, x2, x3, x4, x5, x6, x7)        

instance Tupled 8 (x1, x2, x3, x4, x5, x6, x7, x8) where
    tuple (x1, x2, x3, x4, x5, x6, x7, x8) = (x1, x2, x3, x4, x5, x6, x7, x8)            

    