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
    module X,
    Tuple(..), 
    Tupeler(..), 
    UniTuple(..), 
    UniTupler(..),    
) where
import Alpha.Canonical.Elementary.Common as X
import Alpha.Canonical.Elementary.Set as X

    
        
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

type instance IndexedElement 1 (UniTuple1 a) = a
type instance IndexedElement 1 (Tuple1 a1) = a1
type instance IndexedElement 1 (Tuple2 a1 a2) = a1
type instance IndexedElement 2 (Tuple2 a1 a2) = a2
type instance IndexedElement 1 (Tuple3 a1 a2 a3) = a1
type instance IndexedElement 2 (Tuple3 a1 a2 a3) = a2
type instance IndexedElement 3 (Tuple3 a1 a2 a3) = a3
type instance IndexedElement 1 (Tuple4 a1 a2 a3 a4) = a1
type instance IndexedElement 2 (Tuple4 a1 a2 a3 a4) = a2
type instance IndexedElement 3 (Tuple4 a1 a2 a3 a4) = a3
type instance IndexedElement 4 (Tuple4 a1 a2 a3 a4) = a4
type instance IndexedElement 1 (Tuple5 a1 a2 a3 a4 a5) = a1
type instance IndexedElement 2 (Tuple5 a1 a2 a3 a4 a5) = a2
type instance IndexedElement 3 (Tuple5 a1 a2 a3 a4 a5) = a3
type instance IndexedElement 4 (Tuple5 a1 a2 a3 a4 a5) = a4
type instance IndexedElement 5 (Tuple5 a1 a2 a3 a4 a5) = a5

instance (Eq a1) => NaturallyIndexed 1 (Tuple1 a1) where
    natix (Tuple1 a1) = a1    
    
instance (Eq a1, Eq a2) =>  NaturallyIndexed 1 (Tuple2 a1 a2) where
    natix (a1,_) = a1
instance (Eq a1, Eq a2)  => NaturallyIndexed 2 (Tuple2 a1 a2) where
    natix (_,a2) = a2    

instance (Eq a1, Eq a2, Eq a3)  =>  NaturallyIndexed 1 (Tuple3 a1 a2 a3) where
    natix (a1,_,_) = a1
instance (Eq a1, Eq a2, Eq a3)  =>  NaturallyIndexed 2 (Tuple3 a1 a2 a3) where
    natix (_,a2,_) = a2        
instance (Eq a1, Eq a2, Eq a3)  =>  NaturallyIndexed 3 (Tuple3 a1 a2 a3) where
    natix (_,_,a3) = a3            

instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NaturallyIndexed 1 (Tuple4 a1 a2 a3 a4) where
    natix (a1,_,_,_) = a1
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>   NaturallyIndexed 2 (Tuple4 a1 a2 a3 a4) where
    natix (_,a2,_,_) = a2        
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NaturallyIndexed 3 (Tuple4 a1 a2 a3 a4) where
    natix (_,_,a3,_) = a3                
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NaturallyIndexed 4 (Tuple4 a1 a2 a3 a4) where
    natix (_,_,_,a4) = a4                    

instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5)  => NaturallyIndexed 1 (Tuple5 a1 a2 a3 a4 a5) where
    natix (a1,_,_,_,_) = a1
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 2 (Tuple5 a1 a2 a3 a4 a5) where
    natix (_,a2,_,_,_) = a2        
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 3 (Tuple5 a1 a2 a3 a4 a5) where
    natix (_,_,a3,_,_) = a3                
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 4 (Tuple5 a1 a2 a3 a4 a5) where
    natix (_,_,_,a4,_) = a4                        
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 5 (Tuple5 a1 a2 a3 a4 a5) where
    natix (_,_,_,_,a5) = a5                            
    
instance (Ord a) => SetBuilder (UniTuple1 a) a where
    set (UniTuple1 a1) = fromList [a1] 
instance (Ord a) => SetBuilder (UniTuple2 a) a where
    set (a1,a2) = fromList [a1,a2]
instance (Ord a) => SetBuilder (UniTuple3 a) a where
    set (a1,a2,a3) = fromList [a1,a2,a3]
instance (Ord a) => SetBuilder (UniTuple4 a) a  where
    set (a1,a2,a3,a4) = fromList [a1,a2,a3,a4]
instance (Ord a) => SetBuilder (UniTuple5 a) a where
    set (a1,a2,a3,a4,a5) = fromList [a1,a2,a3,a4,a5] 
    