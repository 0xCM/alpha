{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Data.Tuple
(
    Tuple, Tupled(..),

    Tn(..), T2,T3,T4,T5, T6, T7, T8
)
where
import Alpha.Canonical
import qualified Data.Vector as V

type family Tuple a
type family TuplePlus a

type T2 e = (e, e)
type T3 e = (e, e, e)
type T4 e = (e, e, e, e)
type T5 e = (e, e, e, e, e)
type T6 e = (e, e, e, e, e, e)
type T7 e = (e, e, e, e, e, e, e)    
type T8 e = (e, e, e, e, e, e, e, e)    

data Tn e =
      T2 (T2 e)
    | T3 (T3 e)
    | T4 (T4 e)
    | T5 (T5 e)
    | T6 (T6 e)
    | T7 (T7 e)
    | T8 (T8 e)

class Tupled a where 
    type Tuple' a       
    type Tuple' a = Tuple a
    tuple::a -> Tuple' a
        
type instance Tuple (x1, x2) 
    = (x1, x2)

instance Tupled (x1, x2) where    
    tuple (x1, x2) = (x1, x2)
    
type instance Tuple (x1, x2, x3) 
    = (x1, x2, x3)

instance Tupled (x1, x2, x3) where
    tuple (x1, x2, x3) = (x1, x2, x3)
    
type instance Tuple (x1, x2, x3, x4) 
    = (x1, x2, x3, x4)

instance Tupled (x1, x2, x3, x4) where
    tuple (x1, x2, x3, x4) = (x1, x2, x3, x4)
    
type instance Tuple (x1, x2, x3, x4, x5) 
    = (x1 ,x2, x3, x4, x5)

instance Tupled (x1, x2, x3, x4, x5) where
    tuple (x1, x2, x3, x4, x5) = (x1, x2, x3, x4, x5)
    
type instance Tuple (x1, x2, x3, x4, x5, x6) 
    = (x1 ,x2, x3, x4, x5, x6)

instance Tupled (x1, x2, x3, x4, x5, x6) where
    tuple (x1, x2, x3, x4, x5, x6) = (x1, x2, x3, x4, x5, x6)    
            
type instance Tuple (x1, x2, x3, x4, x5, x6, x7) 
    = (x1 ,x2, x3, x4, x5, x6, x7)

instance Tupled (x1, x2, x3, x4, x5, x6, x7) where
    tuple (x1, x2, x3, x4, x5, x6, x7) = (x1, x2, x3, x4, x5, x6, x7)        

type instance Tuple (x1, x2, x3, x4, x5, x6, x7, x8) 
    = (x1 ,x2, x3, x4, x5, x6, x7, x8)

instance Tupled (x1, x2, x3, x4, x5, x6, x7, x8) where
    tuple (x1, x2, x3, x4, x5, x6, x7, x8) = (x1, x2, x3, x4, x5, x6, x7, x8)            
