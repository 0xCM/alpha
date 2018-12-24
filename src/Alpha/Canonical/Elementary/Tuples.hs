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

    Tuple1(..), Tuple2(..),Tuple3(..),Tuple4(..),Tuple5(..),    
    UniTuple1(..), UniTuple2(..),UniTuple3(..),UniTuple4(..),UniTuple5(..),
    
    UniProduct(..)
    
) where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Elements
import Alpha.Canonical.Elementary.Indexing
import Alpha.Canonical.Elementary.Sets

newtype Tuple1 a1 = Tuple1 a1
    deriving (Eq,Ord)

type Tuple2 a1 a2 = (a1,a2)
type Tuple3 a1 a2 a3 = (a1,a2,a3)
type Tuple4 a1 a2 a3 a4 = (a1,a2,a3,a4)
type Tuple5 a1 a2 a3 a4 a5 = (a1,a2,a3,a4,a5)


type instance Individual (UniTuple1 a) = a
type instance  Individual (UniTuple2 a) = a
type instance Individual (UniTuple3 a) = a
type instance Individual (UniTuple4 a) = a
type instance Individual (UniTuple5 a) = a


newtype UniTuple1 a = UniTuple1 a
    deriving (Eq,Ord)

type UniTuple2 a = Tuple2 a a
type UniTuple3 a = Tuple3 a a a
type UniTuple4 a = Tuple4 a a a a
type UniTuple5 a = Tuple5 a a a a a


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


instance (Eq a) => SetBuilder (UniTuple1 a) a where
    set (UniTuple1 a1) = [a1]
instance (Eq a) => SetBuilder (UniTuple2 a) a where
    set (a1,a2) = [a1,a2]
instance (Eq a) => SetBuilder (UniTuple3 a) a where
    set (a1,a2,a3) = [a1,a2,a3]
instance (Eq a) => SetBuilder (UniTuple4 a) a where
    set (a1,a2,a3,a4) = [a1,a2,a3,a4]
instance (Eq a) => SetBuilder (UniTuple5 a) a where
    set (a1,a2,a3,a4,a5) = [a1,a2,a3,a4,a5]
                

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

instance (Eq a1) => NaturallyIndexed 1 (Tuple1 a1) where
    nix (Tuple1 a1) = a1
    
instance (Eq a1, Eq a2) =>  NaturallyIndexed 1 (Tuple2 a1 a2) where
    nix (a1,_) = a1
instance (Eq a1, Eq a2)  => NaturallyIndexed 2 (Tuple2 a1 a2) where
    nix (_,a2) = a2    

instance (Eq a1, Eq a2, Eq a3)  =>  NaturallyIndexed 1 (Tuple3 a1 a2 a3) where
    nix (a1,_,_) = a1
instance (Eq a1, Eq a2, Eq a3)  =>  NaturallyIndexed 2 (Tuple3 a1 a2 a3) where
    nix (_,a2,_) = a2        
instance (Eq a1, Eq a2, Eq a3)  =>  NaturallyIndexed 3 (Tuple3 a1 a2 a3) where
    nix (_,_,a3) = a3            

instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NaturallyIndexed 1 (Tuple4 a1 a2 a3 a4) where
    nix (a1,_,_,_) = a1
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>   NaturallyIndexed 2 (Tuple4 a1 a2 a3 a4) where
    nix (_,a2,_,_) = a2        
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NaturallyIndexed 3 (Tuple4 a1 a2 a3 a4) where
    nix (_,_,a3,_) = a3                
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NaturallyIndexed 4 (Tuple4 a1 a2 a3 a4) where
    nix (_,_,_,a4) = a4                    

instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5)  => NaturallyIndexed 1 (Tuple5 a1 a2 a3 a4 a5) where
    nix (a1,_,_,_,_) = a1
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 2 (Tuple5 a1 a2 a3 a4 a5) where
    nix (_,a2,_,_,_) = a2        
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 3 (Tuple5 a1 a2 a3 a4 a5) where
    nix (_,_,a3,_,_) = a3                
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 4 (Tuple5 a1 a2 a3 a4 a5) where
    nix (_,_,_,a4,_) = a4                        
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 5 (Tuple5 a1 a2 a3 a4 a5) where
    nix (_,_,_,_,a5) = a5                            

type Formattable2 a1 a2 = (Formattable a1, Formattable a2)
type Formattable3 a1 a2 a3 = (Formattable2 a1 a2, Formattable a3)
type Formattable4 a1 a2 a3 a4 = (Formattable3 a1 a2 a3, Formattable a4)
type Formattable5 a1 a2 a3 a4 a5 = (Formattable4 a1 a2 a3 a4, Formattable a5)

instance Formattable a => Formattable (Tuple1 a) where
    format (Tuple1 a) = format a

instance Formattable a => Formattable (UniTuple1 a) where
    format (UniTuple1 a) = format a
        
instance Formattable a => Show (Tuple1 a) where    
    show = string . format

instance (Formattable2 a1 a2) => Formattable (Tuple2 a1 a2) where
    format (a1,a2)
        = tuplestring [format a1, format a2]
        
instance (Formattable3 a1 a2 a3) => Formattable (Tuple3 a1 a2 a3) where
    format (a1,a2,a3)
        = tuplestring [format a1, format a2, format a3]

instance (Formattable4 a1 a2 a3 a4) => Formattable (Tuple4 a1 a2 a3 a4) where
    format (a1,a2,a3,a4) 
        = tuplestring [format a1, format a2, format a3, format a4]

instance (Formattable5 a1 a2 a3 a4 a5) => Formattable (Tuple5 a1 a2 a3 a4 a5) where
    format (a1,a2,a3,a4,a5) 
        = tuplestring [format a1, format a2, format a3, format a4, format a5]    
