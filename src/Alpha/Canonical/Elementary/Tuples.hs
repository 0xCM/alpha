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

    Tuple2(..),Tuple3(..),Tuple4(..),Tuple5(..),    
    UniTuple2(..),UniTuple3(..),UniTuple4(..),UniTuple5(..),
    
    
) where
import Alpha.Base
import Alpha.Canonical.Elementary.Element
import Alpha.Canonical.Elementary.Elements

type Tuple2 a1 a2 = (a1,a2)
type Tuple3 a1 a2 a3 = (a1,a2,a3)
type Tuple4 a1 a2 a3 a4 = (a1,a2,a3,a4)
type Tuple5 a1 a2 a3 a4 a5 = (a1,a2,a3,a4,a5)

-- Unifies tuple types (for supported arities)
type family Tuple (n::Nat) a = r | r -> a where
    Tuple 2 (a1,a2) = (a1,a2)
    Tuple 3 (a1,a2,a3) = (a1,a2,a3)
    Tuple 4 (a1,a2,a3,a4) = (a1,a2,a3,a4)
    Tuple 5 (a1,a2,a3,a4,a5) = (a1,a2,a3,a4,a5)


type UniTuple2 a = (a,a)
type UniTuple3 a = (a,a,a)
type UniTuple4 a = (a,a,a,a)
type UniTuple5 a = (a,a,a,a,a)
    
type instance Element (UniTuple2 a) = a
type instance Element (UniTuple3 a) = a
type instance Element (UniTuple4 a) = a
type instance Element (UniTuple5 a) = a

type family UniTuple (n::Nat) a = r | r -> a where
    UniTuple 2 a = UniTuple2 a
    UniTuple 3 a = UniTuple3 a
    UniTuple 4 a = UniTuple4 a
    UniTuple 5 a = UniTuple5 a
        
-- Characterizes types from which tuples can be constructed    
class KnownNat n =>  Tupeler n a where
    -- | Forms a tuple from the source value
    tuple::a -> Tuple n a

class KnownNat n => UniTupler n a where
    unituple::a -> UniTuple n a
    
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


