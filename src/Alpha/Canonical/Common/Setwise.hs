-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Common.Setwise
(
    module X,
    FiniteSet(..),
    Universe(..),
    Complementary(..),
    Setwise(..),
    Set(..),
    SetRep(..),    
    powerset,
    set,
    setrep,
)
where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual as X
import Alpha.Canonical.Common.Container as X
import Alpha.Canonical.Common.Conversions as X
import Alpha.Canonical.Common.Format as X
import Alpha.Canonical.Common.Cardinality as X

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set as BS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence
import qualified Numeric.Interval as Interval
import qualified Data.MultiSet as Bag

class Set a where
    -- | Determines whether a given set is a subset
    contains::Bool -> a -> a -> Bool

class (Set a, Nullity a) => Setwise a where

    -- Forms the union of two sets
    union::a -> a -> a 

    -- Forms the intersection of two sets
    intersect::a -> a -> a

    -- Forms the union of an arbitrary (but finite) number of sets
    unions::[a] -> a    
    unions u = reduce h union t where
        (h,t) = (ifelse (null u) empty (List.head u), ifelse (null u) empty (List.tail u))

    -- Forms the union of an arbitrary (but finite) number of sets
    intersections::[a] -> a
    intersections u = ifelse (null u) empty (reduce (List.head u) intersect (List.tail u))
    {-# INLINE intersections #-}
    
    -- Forms the difference of two sets
    diff::a -> a -> a
    diff = (\\)
    {-# INLINE diff #-}

    -- An infix synonym for 'diff' which forms the  difference of two sets
    (\\)::a -> a -> a
    (\\) = diff
    {-# INLINE (\\) #-}
    infix 5 \\

    -- Forms the symmetric difference of two sets
    symdiff::a -> a -> a
    symdiff a b = union (a \\ b) (b \\ a)
    {-# INLINE symdiff #-}

    -- An infix synonym for 'symdiff' which forms the symmetric difference of two sets
    (//\\)::a -> a -> a
    (//\\) = symdiff
    {-# INLINE (//\\) #-}
    infix 5 //\\

-- | A universe is a type that may be populated with and 
-- intrinsic collection of inhabitants    
class (a ~ Individual (FiniteSet a)) => Universe a where
    inhabitants::FiniteSet a
    
class (a ~ Individual (FiniteSet a) , Universe a, Ord a) => Complementary a where
    comp::FiniteSet a -> FiniteSet a 
    comp x = diff inhabitants x

instance (a ~ Individual (FiniteSet a) , Universe a, Ord a) =>  Complementary a

-- | Specifies a set representation that contains either all elements of
-- a specified type or a finite number of elements of said type
newtype SetRep a = SetRep [a]
    deriving (Eq,Ord,Generic,Data,Typeable,Foldable,Traversable,Functor,Applicative,Monad)
    deriving (Membership)
    deriving (Formattable, Show) via ([a])

instance Cardinal (SetRep a) where
    cardinality (SetRep content) = ifelse (null content) Infinite Finite

-- | Represents a (possibly empty) finite set that contains at least one element
newtype FiniteSet a = FiniteSet (BalancedSet a)    
    deriving (Eq,Ord,Generic,Data,Typeable,Foldable)
    deriving (Universal,Existential,Discrete,FinitelyCountable,Set,Setwise)
    deriving (Container,Queryable,Nullity,Cardinal)
    deriving (Formattable) via (BalancedSet a)
instance Newtype(FiniteSet a)

-- | Represents a union of an homogenous collection of sets
newtype Union a = Union [FiniteSet a]
    deriving(Eq,Ord,Generic,Data,Typeable)
    
-- | Represents an intersection of an homogenous collection of sets
newtype Intersection a = Intersection [FiniteSet a]
    deriving(Eq,Ord,Generic,Data,Typeable)

type instance Individual (Set a) = a
type instance Individual (FiniteSet a) = a
type instance Individual (SetRep a) = a
    
powerset::Ord a => FiniteSet a -> FiniteSet (FiniteSet a)
powerset (FiniteSet s) = FiniteSet $ Set.map FiniteSet (Set.powerSet s) 
        
set::Ord a => [a] -> FiniteSet a
set = FiniteSet . fromList

-- | Constructs a set representative
setrep::[a] -> SetRep a
setrep = SetRep

-- * Set instances
-------------------------------------------------------------------------------                        
instance Ord a => Set (SetRep a) where    
    contains proper candidate (SetRep []) = True
    contains proper (SetRep candidate) (SetRep source) = contains proper candidate source
    
instance Ord a => Set (BalancedSet a) where
    contains proper candidate source 
        = ifelse proper  
            (Set.isProperSubsetOf c' s')  
            (Set.isSubsetOf c' s')  
                where (c', s') = (candidate, source)

instance Ord a => Set [a] where        
    contains proper candidate source  
        = test (Set.fromList candidate) (Set.fromList source)
            where test = ifelse proper Set.isProperSubsetOf Set.isSubsetOf 
        
instance Ord a => Set (Bag a) where    
    contains proper candidate source 
        = ifelse proper 
            (Bag.isProperSubsetOf candidate source) 
            (Bag.isSubsetOf candidate source)
                
-- * FinitelyCountable instances
-------------------------------------------------------------------------------                        
instance Ord a => Discrete (BalancedSet a) where
    individuals s = s |> Set.toList

instance Ord a => FinitelyCountable (BalancedSet a) where
    count  = fromIntegral . Set.size 

                    
-- * Existential instances
-------------------------------------------------------------------------------            
instance Existential (BalancedSet a) where
    any pred s = s |> Set.toList |> List.any pred
    
-- * Universal instances
-------------------------------------------------------------------------------            
instance Universal (BalancedSet a) where
    all pred s = s |> Set.toList |> List.all pred

-- * Queryable instances
-------------------------------------------------------------------------------            
instance Ord a => Queryable (BalancedSet a) where
    filter p s = s |> toList |> List.filter p

-------------------------------------------------------------------------------            
-- * Setwise instances
-------------------------------------------------------------------------------            
instance Ord a => Setwise (BalancedSet a) where
    union = Set.union
    intersect = Set.intersection 
    diff = (\\)

instance Ord a => Setwise [a] where    
    union = List.union
    intersect = List.intersect
    diff =  (List.\\)
 
instance Ord a => Setwise (Bag a) where    
    union = Bag.union        
    intersect = Bag.intersection
    diff = Bag.difference
    

-- * JoinableMeet instances
-------------------------------------------------------------------------------                        
instance Ord a => JoinableMeet (BalancedSet a)  where
    (\/) = union
    (/\) = intersect

instance Ord a => JoinableMeet (Bag a)  where
    (\/) = union
    (/\) = intersect
    
instance Ord a => JoinableMeet [a]  where
    (\/) = union
    (/\) = intersect
            

-- * Cardinal instances
-------------------------------------------------------------------------------            
instance Ord a => Cardinal (BalancedSet a) where
    cardinality s = ifelse (null s) Zero Finite

instance Ord a => Cardinal (Union a) where
    cardinality (Union sets) = result where
        totalCt = List.length sets
        finiteCt = (\s -> ifelse (isFinite s) 1 0) <$> sets |> List.sum
        emptyCt = (\s -> ifelse (isEmpty s) 1 0) <$> sets |> List.sum
        result = ifelse (emptyCt == totalCt) Zero (ifelse (finiteCt == totalCt) Finite Infinite)

-- *Mappable instances
-------------------------------------------------------------------------------            

instance OrdPair a b => Mappable(FiniteSet a) a b where
    type Mapped (FiniteSet a) a b = FiniteSet b
    map f (FiniteSet s) = FiniteSet $ map f s
        
-- * IsList instances
-------------------------------------------------------------------------------            
instance Ord a => IsList (FiniteSet a) where
    type Item (FiniteSet a) = a
    toList (FiniteSet s) = toList s    
    fromList (xs) = FiniteSet (fromList xs)

-- *Formattable instances
-------------------------------------------------------------------------------            
instance (Formattable a, Ord a) => Formattable (BalancedSet a) where
    format s = setstring (toList s)
        
instance (Formattable a, Ord a) => Show (FiniteSet a) where
    show = Text.unpack . format            

-- *Universe instances
-------------------------------------------------------------------------------            
instance Universe Int8 where
    inhabitants = set ivalues
instance Universe Int16 where
    inhabitants = set ivalues
instance Universe Word8 where
    inhabitants = set ivalues
instance Universe Word16 where
    inhabitants = set ivalues
instance Universe Bool where
    inhabitants = set ivalues