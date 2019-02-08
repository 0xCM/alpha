-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Common.Tuples
(
    module X,
    Tuple1(..), 
    Tuple(..),
    UniTuple1(..), 
    UniTuple(..),
    Tupeler(..), 
    UniTupler(..),    
    TupleIndexed(..),
    TupleIx(..),

) where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual as X
import Alpha.Canonical.Common.Synonyms as X
import Alpha.Canonical.Common.Indexing as X

-- | Characterizes a type indexed by a tuple
class  (KnownNat i, OrdEnum a) => TupleIndexed i a where
    tupleix::UniTuple i (IxRange a) -> TupleIx i a
    ixlevels::TupleIx i a -> [UniTuple i a]

newtype Tuple1 a1 = Tuple1 a1
    deriving (Eq,Ord)

newtype UniTuple1 a = UniTuple1 a
    deriving (Eq,Ord)

-- Unifies tuple types (for supported arities)
type family Tuple (n::Nat) a = r | r -> a where
    Tuple 1 a1 = Tuple1 a1
    Tuple 2 (a1,a2) = (a1,a2)
    Tuple 3 (a1,a2,a3) = (a1,a2,a3)
    Tuple 4 (a1,a2,a3,a4) = (a1,a2,a3,a4)
    Tuple 5 (a1,a2,a3,a4,a5) = (a1,a2,a3,a4,a5)

type family UniTuple (n::Nat) a = r | r -> a where
    UniTuple 1 a = UniTuple1 a
    UniTuple 2 a = UniTuple2 a
    UniTuple 3 a = UniTuple3 a
    UniTuple 4 a = UniTuple4 a
    UniTuple 5 a = UniTuple5 a

-- | Defines a family of multi-indexes    
type family TupleIx (i::Nat) a = r | r -> i a where
    TupleIx 1 a = UniTuple1 (IxRange a)
    TupleIx 2 a = UniTuple2 (IxRange a)
    TupleIx 3 a = UniTuple3 (IxRange a)
    TupleIx 4 a = UniTuple4 (IxRange a)
    TupleIx 5 a = UniTuple5 (IxRange a)

type instance Individual (UniTuple1 a) = a
type instance Individual (UniTuple2 a) = a
type instance Individual (UniTuple3 a) = a
type instance Individual (UniTuple4 a) = a
type instance Individual (UniTuple5 a) = a


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

instance (Eq a) => Vectored (UniTuple1 a) where
    vector (UniTuple1 a1) = [a1]
        
instance (Eq a) => Vectored (UniTuple2 a) where
    vector (a1,a2) = [a1,a2]

instance (Eq a) => Vectored (UniTuple3 a) where    
    vector (a1,a2,a3) = [a1,a2,a3]    

instance (Eq a) => Vectored (UniTuple4 a) where
    vector (a1,a2,a3,a4) = [a1,a2,a3,a4]    

instance (Eq a) => Vectored (UniTuple5 a) where
    vector (a1,a2,a3,a4,a5) = [a1,a2,a3,a4,a5]    

instance Formattable a => Formattable (Tuple1 a) where
    format (Tuple1 a) = format a

instance Formattable a => Formattable (UniTuple1 a) where
    format (UniTuple1 a) = format a

instance Formattable a => Show (UniTuple1 a) where    
    show = string . format
        
instance Formattable a => Show (Tuple1 a) where    
    show = string . format

-------------------------------------------------------------------------------            
-- * NatIx instances
-------------------------------------------------------------------------------            

instance (Eq a1) => NatIx 1 (Tuple1 a1) where
    type NatIndexed 1 (Tuple1 a1) = a1
    natix (Tuple1 a1) = a1    
    
instance (Eq a1, Eq a2) =>  NatIx 1 (Tuple2 a1 a2) where
    type NatIndexed 1 (Tuple2 a1 a2) = a1
    natix (a1,_) = a1

instance (Eq a1, Eq a2)  => NatIx 2 (Tuple2 a1 a2) where
    type NatIndexed 2 (Tuple2 a1 a2) = a2
    natix (_,a2) = a2    

instance (Eq a1, Eq a2, Eq a3)  =>  NatIx 1 (Tuple3 a1 a2 a3) where
    type NatIndexed 1 (Tuple3 a1 a2 a3) = a1
    natix (a1,_,_) = a1
instance (Eq a1, Eq a2, Eq a3)  =>  NatIx 2 (Tuple3 a1 a2 a3) where
    type NatIndexed 2 (Tuple3 a1 a2 a3) = a2
    natix (_,a2,_) = a2        
instance (Eq a1, Eq a2, Eq a3)  =>  NatIx 3 (Tuple3 a1 a2 a3) where
    type NatIndexed 3 (Tuple3 a1 a2 a3) = a3
    natix (_,_,a3) = a3            

instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NatIx 1 (Tuple4 a1 a2 a3 a4) where
    type NatIndexed 1 (Tuple4 a1 a2 a3 a4) = a1
    natix (a1,_,_,_) = a1
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>   NatIx 2 (Tuple4 a1 a2 a3 a4) where
    type NatIndexed 2 (Tuple4 a1 a2 a3 a4) = a2
    natix (_,a2,_,_) = a2        
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NatIx 3 (Tuple4 a1 a2 a3 a4) where
    type NatIndexed 3 (Tuple4 a1 a2 a3 a4) = a3
    natix (_,_,a3,_) = a3                
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NatIx 4 (Tuple4 a1 a2 a3 a4) where
    type NatIndexed 4 (Tuple4 a1 a2 a3 a4) = a4
    natix (_,_,_,a4) = a4                    

instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5)  => NatIx 1 (Tuple5 a1 a2 a3 a4 a5) where
    type NatIndexed 1 (Tuple5 a1 a2 a3 a4 a5) = a1
    natix (a1,_,_,_,_) = a1
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NatIx 2 (Tuple5 a1 a2 a3 a4 a5) where
    type NatIndexed 2 (Tuple5 a1 a2 a3 a4 a5) = a2
    natix (_,a2,_,_,_) = a2        
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NatIx 3 (Tuple5 a1 a2 a3 a4 a5) where
    type NatIndexed 3 (Tuple5 a1 a2 a3 a4 a5) = a3
    natix (_,_,a3,_,_) = a3                
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NatIx 4 (Tuple5 a1 a2 a3 a4 a5) where
    type NatIndexed 4 (Tuple5 a1 a2 a3 a4 a5) = a4
    natix (_,_,_,a4,_) = a4                        
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NatIx 5 (Tuple5 a1 a2 a3 a4 a5) where
    type NatIndexed 5 (Tuple5 a1 a2 a3 a4 a5) = a5
    natix (_,_,_,_,a5) = a5                            

-- * Discrete instances
-------------------------------------------------------------------------------                
instance Ord a => Discrete (UniTuple1 a) where
    individuals (UniTuple1 a1) = fromList [a1] 
instance (Ord a) => Discrete (UniTuple2 a) where
    individuals (a1,a2) = fromList [a1,a2]
instance (Ord a) => Discrete (UniTuple3 a) where
    individuals (a1,a2,a3) = fromList [a1,a2,a3]
instance (Ord a) => Discrete (UniTuple4 a)  where
    individuals (a1,a2,a3,a4) = fromList [a1,a2,a3,a4]
instance (Ord a) => Discrete (UniTuple5 a) where
    individuals (a1,a2,a3,a4,a5) = fromList [a1,a2,a3,a4,a5] 
    
-- * TupleIndexed instances
-------------------------------------------------------------------------------                
instance (OrdEnum a) => TupleIndexed 1 a where    
    tupleix(UniTuple1 r) =  UniTuple1 r
    ixlevels (UniTuple1 r) = [UniTuple1 a | a <- individuals r]

instance (OrdEnum a) => TupleIndexed 2 a where    
    tupleix(r1, r2) = (r1, r2)
    ixlevels (r1, r2) 
        = [(a1,a2) | a1 <- individuals r1, a2 <- individuals r2]
                    
instance (OrdEnum a) => TupleIndexed 3 a where    
    tupleix(r1, r2, r3) = (r1 , r2, r3)
    ixlevels (r1, r2, r3) 
        = [(a1,a2,a3) | a1 <- individuals r1, a2 <- individuals r2, a3 <- individuals r3]

instance (OrdEnum a) => TupleIndexed 4 a where    
    tupleix(r1, r2, r3, r4) = (r1 , r2, r3, r4)
    ixlevels (r1, r2, r3, r4) 
        =  [(a1,a2,a3,a4) | 
                a1 <- individuals r1, 
                a2 <- individuals r2, 
                a3 <- individuals r3, 
                a4 <- individuals r4]

instance (OrdEnum a) => TupleIndexed 5 a where    
    tupleix(r1, r2, r3, r4, r5) = (r1 , r2, r3, r4, r5)
    ixlevels (r1, r2, r3, r4, r5) 
        =  [(a1,a2,a3,a4,a5) | 
                a1 <- individuals r1, 
                a2 <- individuals r2, 
                a3 <- individuals r3, 
                a4 <- individuals r4, 
                a5 <- individuals r5]        