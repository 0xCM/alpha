-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Elementary.Tuples
(    
    Tuple(..), Tupeler(..), 
    UniTuple(..), UniTupler(..),

    
    UniProduct(..)
    
) where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Indexed
import Alpha.Canonical.Elementary.Set
    

type family UniProduct (n::Nat) a = r | r -> n where 
    UniProduct 1 a = UniTuple1 a
    UniProduct 2 a = UniTuple2 a
    UniProduct 3 a = UniTuple3 a
    UniProduct 4 a = UniTuple4 a
    UniProduct 5 a = UniTuple5 a
        
-- Characterizes types from which tuples can be constructed    
class KnownNat n =>  Tupeler n a where
    -- | Forms a tuple from the source value
    tuple::a -> Tuple n a

-- Characterizes types from which tuples can be constructed    
class KnownNat n =>  UniTupler n a where
    -- | Forms a tuple from the source value
    unituple::a -> UniTuple n a

instance Tupeler 1 (Tuple1 a1) where
    tuple a1  = Tuple1 a1
    {-# INLINE tuple #-}
        
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


                

instance (Eq a) => Vectored (UniTuple1 a) a where
    vector (UniTuple1 a1) = [a1]
        
instance (Eq a) => Vectored (UniTuple2 a) a where
    vector (a1,a2) = [a1,a2]

instance (Eq a) => Vectored (UniTuple3 a) a where    
    vector (a1,a2,a3) = [a1,a2,a3]    

instance (Eq a) => Vectored (UniTuple4 a) a where
    vector (a1,a2,a3,a4) = [a1,a2,a3,a4]    

instance (Eq a) => Vectored (UniTuple5 a) a where
    vector (a1,a2,a3,a4,a5) = [a1,a2,a3,a4,a5]    



instance Formattable a => Formattable (Tuple1 a) where
    format (Tuple1 a) = format a

instance Formattable a => Formattable (UniTuple1 a) where
    format (UniTuple1 a) = format a

instance Formattable a => Show (UniTuple1 a) where    
    show = string . format
        
instance Formattable a => Show (Tuple1 a) where    
    show = string . format

