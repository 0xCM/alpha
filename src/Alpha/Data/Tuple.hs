{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Data.Tuple where
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Combinators

import qualified Data.Vector as V

type family Tuple a        
type instance Tuple (x1, x2) = (x1, x2)
type instance Tuple (x1, x2, x3) = (x1, x2, x3)
type instance Tuple (x1, x2, x3, x4) = (x1, x2, x3, x4)
type instance Tuple (x1, x2, x3, x4, x5)  = (x1 ,x2, x3, x4, x5)
type instance Tuple (x1, x2, x3, x4, x5, x6)  = (x1 ,x2, x3, x4, x5, x6)
type instance Tuple (x1, x2, x3, x4, x5, x6, x7)  = (x1 ,x2, x3, x4, x5, x6, x7)
type instance Tuple (x1, x2, x3, x4, x5, x6, x7, x8)  = (x1 ,x2, x3, x4, x5, x6, x7, x8)
type instance Tuple (x1, x2, x3, x4, x5, x6, x7, x8, x9)  = (x1 ,x2, x3, x4, x5, x6, x7, x8, x9)

class Tupled (n::Nat) a where 
    tuple::a -> Tuple a

data T2 x1 x2 = T2 {-# UNPACK #-} !(x1, x2)
    deriving (Eq, Ord, Data,Generic, Typeable, Functor)

instance (Formattable x1, Formattable x2) => Formattable (T2 x1 x2) where
    format (T2 (x1, x2)) = tupelize [format x1, format x2]

data T3 x1 x2 x3 = T3 {-# UNPACK #-} !(x1,x2,x3)
    deriving (Eq, Ord, Data,Generic, Typeable, Show)

data T4 x1 x2 x3 x4 = T4 {-# UNPACK #-} !(x1,x2,x3,x4)
    deriving (Eq, Ord, Data,Generic, Typeable, Show)

data T5 x1 x2 x3 x4 x5 = T5 {-# UNPACK #-} !(x1,x2,x3,x4,x5)
    deriving (Eq, Ord, Data,Generic, Typeable, Show)

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

instance Tupled 8 (x1, x2, x3, x4, x5, x6, x7, x8, x9) where
    tuple (x1, x2, x3, x4, x5, x6, x7, x8, x9) = (x1, x2, x3, x4, x5, x6, x7, x8, x9)            
        
