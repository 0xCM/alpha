{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Relations.Tuples
(    
    Tupled(..), Tuple2(..),Tuple3(..),Tuple4(..),Tuple5(..),
    UniTupled(..), UniTuple2(..),UniTuple3(..),UniTuple4(..),UniTuple5(..),
    Tupeler(..),

) where
import Alpha.Base
import Alpha.Canonical.Element

type Tuple2 a1 a2 = (a1,a2)
type Tuple3 a1 a2 a3 = (a1,a2,a3)
type Tuple4 a1 a2 a3 a4 = (a1,a2,a3,a4)
type Tuple5 a1 a2 a3 a4 a5 = (a1,a2,a3,a4,a5)

-- Represents a family of types that can be represented as tuples
type family Tupled (n::Nat) a = r | r -> a where
    Tupled 2 (Tuple2 a1 a2) = Tuple2 a1 a2
    Tupled 3 (Tuple3 a1 a2 a3) = Tuple3 a1 a2 a3
    Tupled 4 (Tuple4 a1 a2 a3 a4) = Tuple4 a1 a2 a3 a4
    Tupled 5 (Tuple5 a1 a2 a3 a4 a5) = Tuple5 a1 a2 a3 a4 a5

type UniTuple2 a = Tuple2 a a
type UniTuple3 a = Tuple3 a a a
type UniTuple4 a = Tuple4 a a a a
type UniTuple5 a = Tuple5 a a a a a

type instance Element (UniTuple2 a) = a
type instance Element (UniTuple3 a) = a
type instance Element (UniTuple4 a) = a
type instance Element (UniTuple5 a) = a

type family UniTupled (n::Nat) a = r | r -> a where
    UniTupled 2 (a,a) = (a,a)
    UniTupled 3 (a,a,a) = (a,a,a)
    UniTupled 4 (a,a,a,a) = (a,a,a,a)
    UniTupled 5 (a,a,a,a,a) = (a,a,a,a,a)


-- Characterizes types from which tuples can be constructed    
class KnownNat n =>  Tupeler n a where
    -- | Forms a tuple from the source value
    tuple::a -> Tupled n a

class KnownNat n => UniTupler n a where
    unituple::a -> UniTupled n a
    
instance Tupeler 2 (Tuple2 a1 a2 ) where
    tuple (a1,a2)  = (a1,a2)
    {-# INLINE tuple #-}

instance Tupeler 3 (Tuple3 a1 a2 a3) where
    tuple (a1,a2,a3)  = (a1,a2,a3)
    {-# INLINE tuple #-}

instance Tupeler 4  (Tuple4 a1 a2 a3 a4) where
    tuple (a1,a2,a3,a4)  = (a1,a2,a3,a4)
    {-# INLINE tuple #-}

instance Tupeler 5 (Tuple5 a1 a2 a3 a4 a5) where
    tuple (a1,a2,a3,a4,a5)  = (a1,a2,a3,a4,a5)
    {-# INLINE tuple #-}
            
instance UniTupler 2 (UniTuple2 a) where
    unituple (a1,a2) = (a1,a2)

instance UniTupler 3 (UniTuple3 a) where
    unituple (a1,a2,a3) = (a1,a2,a3)    

instance UniTupler 4 (UniTuple4 a) where
    unituple (a1,a2,a3,a4) = (a1,a2,a3,a4)        

instance UniTupler 5 (UniTuple5 a) where
    unituple (a1,a2,a3,a4,a5) = (a1,a2,a3,a4,a5)            

instance Listing (UniTuple2 a) where
    list (a1,a2) = [a1,a2]

instance Listing (UniTuple3 a) where    
    list (a1,a2,a3) = [a1,a2,a3]    

instance Listing (UniTuple4 a) where
    list (a1,a2,a3,a4) = [a1,a2,a3,a4]    

instance Listing (UniTuple5 a) where
    list (a1,a2,a3,a4,a5) = [a1,a2,a3,a4,a5]    
    
instance Vectored (UniTuple2 a) where
    vector (a1,a2) = [a1,a2]

instance Vectored (UniTuple3 a) where    
    vector (a1,a2,a3) = [a1,a2,a3]    

instance Vectored (UniTuple4 a) where
    vector (a1,a2,a3,a4) = [a1,a2,a3,a4]    

instance Vectored (UniTuple5 a) where
    vector (a1,a2,a3,a4,a5) = [a1,a2,a3,a4,a5]    
        