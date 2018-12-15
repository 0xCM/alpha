module Alpha.Canonical.Relations.Tuples
(    
    Tupled(..), Tuple2,Tuple3,Tuple4,Tuple5,
    UniTupled(..), UniTuple2,UniTuple3,UniTuple4,UniTuple5,
    Tupeler(..),

) where
import Alpha.Base

type Tuple2 a1 a2 = (a1,a2)
type Tuple3 a1 a2 a3 = (a1,a2,a3)
type Tuple4 a1 a2 a3 a4 = (a1,a2,a3,a4)
type Tuple5 a1 a2 a3 a4 a5 = (a1,a2,a3,a4,a5)

-- Represents a family of types that can be represented as tuples
type family Tupled a = r | r -> a where
    Tupled (Tuple2 a1 a2) = Tuple2 a1 a2
    Tupled (Tuple3 a1 a2 a3) = Tuple3 a1 a2 a3
    Tupled (Tuple4 a1 a2 a3 a4) = Tuple4 a1 a2 a3 a4
    Tupled (Tuple5 a1 a2 a3 a4 a5) = Tuple5 a1 a2 a3 a4 a5

type UniTuple2 a = Tuple2 a a
type UniTuple3 a = Tuple3 a a a
type UniTuple4 a = Tuple4 a a a a
type UniTuple5 a = Tuple5 a a a a a
    
type family UniTupled a = r | r -> a where
    UniTupled (UniTuple2 a) = UniTuple2 a
    UniTupled (UniTuple3 a) = UniTuple3 a
    UniTupled (UniTuple4 a) = UniTuple4 a
    UniTupled (UniTuple5 a) = UniTuple5 a


-- Characterizes types from which tuples can be constructed    
class Tupeler a where
    -- | Forms a tuple from the source value
    tuple::a -> Tupled a

instance Tupeler (Tuple2 a1 a2 ) where
    tuple (a1,a2)  = (a1,a2)
    {-# INLINE tuple #-}

instance Tupeler (Tuple3 a1 a2 a3) where
    tuple (a1,a2,a3)  = (a1,a2,a3)
    {-# INLINE tuple #-}

instance Tupeler (Tuple4 a1 a2 a3 a4) where
    tuple (a1,a2,a3,a4)  = (a1,a2,a3,a4)
    {-# INLINE tuple #-}

instance Tupeler (Tuple5 a1 a2 a3 a4 a5) where
    tuple (a1,a2,a3,a4,a5)  = (a1,a2,a3,a4,a5)
    {-# INLINE tuple #-}
        
class UniTupler a where
    unituple::a -> UniTupled a
    
instance UniTupler (UniTuple2 a) where
    unituple (a1,a2) = (a1,a2)

instance UniTupler (UniTuple3 a) where
    unituple (a1,a2,a3) = (a1,a2,a3)    

instance UniTupler (UniTuple4 a) where
    unituple (a1,a2,a3,a4) = (a1,a2,a3,a4)        

instance UniTupler (UniTuple5 a) where
    unituple (a1,a2,a3,a4,a5) = (a1,a2,a3,a4,a5)            

-- instance Functor (UniTuple2 a) where
--     fmap f (a1,a2) = (f a1, f a2)