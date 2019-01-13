-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Elementary.Set
(
    module X,
    SetBuilder(..),
    Set(..),
    FneSet(..),
    SetInfo(..),
    Universe(..),
    Complementary(..),
    Unionizable(..),
    Difference(..),
    Intersectable(..),    
    Containment(..),
    emptyset, 
    finset,
    fneset,
)
where
import Alpha.Canonical.Elementary.Common as X

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence
import qualified Numeric.Interval as Interval
import qualified Data.MultiSet as Bag

-- | Represents a finite set that contains at least one element
newtype FneSet a = FneSet (Set' a)    
    deriving (Eq,Ord,Generic,Data,Typeable)
instance Newtype(FneSet a)    

newtype SetInfo a = SetInfo (Cardinality, [a])    
    deriving (Eq,Ord,Generic,Data,Typeable)

data Set a
    = EmptySet
    | FiniteSet (Set' a) 
    | InfiniteSet [a]
    deriving(Generic, Data, Typeable, Eq, Ord)    

type instance Individual (FneSet a) = a
type instance Individual (Set a) = a
type instance Individual (SetInfo a) = a

class Unionizable a where        
    -- The union operator
    union::a -> a -> a 

    unions::[a] -> a
    default unions::(Nullity a, Unionizable a) => [a] -> a
    unions u = reduce empty union u


class Difference a where
    diff::a -> a -> a

    (\\)::a -> a -> a
    (\\) = diff
    {-# INLINE (\\) #-}
    infix 5 \\
        
class Intersectable a  where
    intersect::a -> a -> a

class Containment a where
    isSubset::Bool -> a -> a -> Bool
        
class Subset a where
    subset::Set a -> Set a
    subset = id

class (a ~ Individual b) =>  SetBuilder b a where
    set::b -> Set a

-- | A universe is a type that may be populated with and 
-- intrinsic collection of inhabitants    
class (a ~ Individual (Set a) ) => Universe a where
    inhabitants::Set a
    
class (a ~ Individual (Set a) , Universe a, Ord a) => Complementary a where
    comp::Set a -> Set a 
    comp x = diff inhabitants x

instance (a ~ Individual (Set a) , Universe a, Ord a) =>  Complementary a

-- | Constructs the empty set
emptyset::Set a
emptyset = EmptySet

-- | Constructs a finite set
finset::(Ord a) => [a] -> Set a
finset = FiniteSet . fromList

-- | Constructs an infinite set 
infiniteSet::(Ord a) => [a] -> Set a
infiniteSet = InfiniteSet

isFinite::Set a -> Bool
isFinite (FiniteSet x) = True
isFinite _ = False

isInfinite::Set a -> Bool
isInfinite (InfiniteSet a) = True
isInfinite _ = False

isEmpty::Set a -> Bool
isEmpty EmptySet = True
isEmpty _ = False


-- | Constructs a powerset
powerset::Ord a => Set a -> Set(Set a)
powerset (FiniteSet src) = Set.map FiniteSet (powerset' src) |> FiniteSet
powerset _ = EmptySet

fneset::(Ord a) => NonEmpty a -> FneSet a
fneset (a :| xs) = FneSet (Set.fromList(a : xs))

    
-------------------------------------------------------------------------------            
-- * FneSet class membership
-------------------------------------------------------------------------------                
instance Universal (FneSet a) where
    all pred (FneSet s) = s |> Set.toList |> List.all pred

instance Existential (FneSet a) where
    any pred (FneSet s) = s |> Set.toList |> List.any pred
        
instance Ord a => IsList (FneSet a) where
    type Item (FneSet a) = a
    toList (FneSet s) = toList s

    fromList ([]) = error "no"
    fromList (xs) = FneSet (fromList xs)

instance Ord a => Associated (FneSet a) where
    associates (FneSet s) = toList s

instance Ord a => Discrete (FneSet a) where
    individuals = associates        

instance Ord a => Finite (FneSet a)
    
instance (Ord a) => Unionizable (FneSet a) where
    union (FneSet x) (FneSet y )  =  FneSet $ Set.union x y  
    unions = undefined
    
instance Ord a => Difference (FneSet a) where
    diff (FneSet x) (FneSet y) = FneSet $ x \\ y
    
instance Ord a => Intersectable (FneSet a) where
    intersect (FneSet x) (FneSet y) = FneSet $ Set.intersection x y 
                
instance Ord a => Containment (FneSet a) where
    isSubset proper (FneSet candidate) (FneSet source) 
        = ifelse proper  
            (Set.isProperSubsetOf c' s')  
            (Set.isSubsetOf c' s')  
                where (c', s') = (candidate, source)
    
instance (Ord a) => Queryable (FneSet a) where
    filter p (FneSet s) = s |> toList |> List.filter p

instance (Ord a, Ord b) => Mappable(FneSet a) a b where
    type Mapped (FneSet a) a b = FneSet b
    map f (FneSet s) = FneSet $ Set.map f s
        
instance (Formattable a, Ord a) => Formattable (FneSet a) where
    format (FneSet l) = setstring (toList l) where

instance (Formattable a, Ord a) => Show (FneSet a) where
    show = Text.unpack . format            

-------------------------------------------------------------------------------            
-- * Set class membership
-------------------------------------------------------------------------------
instance (Ord a, Ord b) => Mappable(Set a) a b where
    type Mapped (Set a) a b = Set b
    map f (FiniteSet s) = FiniteSet $ Set.map f s
    map f (EmptySet) = EmptySet
    map f (InfiniteSet x) =  InfiniteSet $ List.map f x

instance (Formattable a, Ord a) => Show (Set a) where
    show = Text.unpack . format

instance Ord a => Nullity (Set a) where
    empty = EmptySet
    null (EmptySet) = True
    null _ = False
        
instance (Ord a) => Queryable (Set a) where
    filter p (FiniteSet s) = s |> toList |> List.filter p
    filter p (InfiniteSet s) = List.filter p s        

instance (Ord a) => Cardinal (Set a) where
    cardinality EmptySet = Zero
    cardinality (InfiniteSet s) = Infinite
    cardinality s = Finite

instance (Ord a) => Descriptor (Set a) (SetInfo a) where
    describe EmptySet  = SetInfo (Zero,  [])
    describe (FiniteSet s) = SetInfo(Finite, toList s)
    describe (InfiniteSet s) = SetInfo (Infinite, s)

instance Ord a => IsList (Set a) where
    type Item (Set a) = a
    toList (FiniteSet s) = toList s
    toList (InfiniteSet s) = s
    toList EmptySet = []

    fromList (xs) = FiniteSet (fromList xs)    

instance Ord a => Associated (Set a) where
    associates (FiniteSet s) = toList s
    associates (InfiniteSet s) = s
    associates (_) = []
    
instance Ord a => Discrete (Set a) where
    individuals = associates

instance Ord a => Finite (Set a) where
    count = fromIntegral . List.length . associates
        
instance (Ord a) => Unionizable (Set a) where
    union EmptySet x = x
    union x EmptySet = x
    union (FiniteSet x) (FiniteSet y)  =  FiniteSet $ Set.union x y    
    union (InfiniteSet x)  (InfiniteSet y) = InfiniteSet $ List.union x y
    union (InfiniteSet x)  y = InfiniteSet $ List.union (toList y) x
    union x  (InfiniteSet y) = InfiniteSet $ List.union (toList x) y
    
instance Universal (Set a) where
    all pred EmptySet = True
    all pred (FiniteSet s) = s |> Set.toList |> List.all pred
    all pred (InfiniteSet x) = List.all pred x
    
instance Existential (Set a) where
    any pred (FiniteSet x) = x |> Set.toList |> List.any pred
    any pred (EmptySet) = False    
    any pred (InfiniteSet x) = List.any pred x

instance Ord a => Containment (Set a) where
    isSubset proper (FiniteSet candidate) (FiniteSet source)  = isSubset proper candidate source
    isSubset proper EmptySet _ = True
    isSubset proper _ EmptySet = False
    isSubset proper (InfiniteSet x) (InfiniteSet y) = List.intersect x y == x
    isSubset proper (InfiniteSet x) (FiniteSet y) = False
    isSubset proper x (InfiniteSet y) =  List.intersect (toList x) y |> fromList |> (==) x

instance Ord a => Difference (Set a) where
    diff (FiniteSet x) (FiniteSet y) = FiniteSet $ x \\ y
    diff (InfiniteSet x) (InfiniteSet y) = InfiniteSet $ x \\ y
    diff (FiniteSet x) (InfiniteSet y) = InfiniteSet $ (list x) \\ y
    diff (InfiniteSet x) (FiniteSet y) = InfiniteSet $ x\\ (list y)
    diff x EmptySet = x
    diff EmptySet x = EmptySet

instance Ord a => Intersectable (Set a) where
    intersect (FiniteSet x) (FiniteSet y) = FiniteSet $ Set.intersection x y 

instance (Formattable a, Ord a) => Formattable (Set a) where
    format  EmptySet =  LBrace <> RBrace
    format  (InfiniteSet x) =  LBrace <> RBrace
    format (FiniteSet l) = setstring (toList l) where
        
    
-------------------------------------------------------------------------------            
-- * Universe instances
-------------------------------------------------------------------------------            
instance Universe Int8 where
    inhabitants = FiniteSet (fromList enumerate)
instance Universe Int16 where
    inhabitants = FiniteSet (fromList enumerate)
instance Universe Int32 where
    inhabitants = FiniteSet (fromList enumerate)
instance Universe Int64 where
    inhabitants = FiniteSet (fromList enumerate)
instance Universe Word8 where
    inhabitants = FiniteSet (fromList enumerate)
instance Universe Word16 where
    inhabitants = FiniteSet (fromList enumerate)
instance Universe Word32 where
    inhabitants = FiniteSet (fromList enumerate)
instance Universe Word64 where
    inhabitants = FiniteSet (fromList enumerate)
instance Universe Natural where
    inhabitants = InfiniteSet [0..]
instance Universe Bool where
    inhabitants = FiniteSet (fromList enumerate)

-------------------------------------------------------------------------------            
-- SetBuilder instances
-------------------------------------------------------------------------------            
instance (Ord a) => SetBuilder [a] a where
    set src = ifelse (List.null src) EmptySet (FiniteSet (fromList src))
    
instance (Ord a) => SetBuilder (SetInfo a) a where
    set (SetInfo (Zero, items)) = EmptySet
    set (SetInfo (Finite, items)) = FiniteSet (fromList items)
    set (SetInfo (Infinite, items)) = InfiniteSet items

-------------------------------------------------------------------------------            
-- * Unionizable instances
-------------------------------------------------------------------------------            
    
instance (Ord a) => Unionizable [a] where    
    union = List.union
            
instance (Ord a) => Unionizable (Bag a) where    
    union = Bag.union        

-------------------------------------------------------------------------------            
-- * Difference instances
-------------------------------------------------------------------------------            
instance (Ord a) => Difference (Bag a) where
    diff = Bag.difference

instance (Eq a, Ord a) => Difference [a] where        
    diff =  (List.\\)

instance (Ord a) => Difference (Set' a)  where        
    diff =  (Set.\\)

-------------------------------------------------------------------------------            
-- * Intersectable instances
-------------------------------------------------------------------------------            
instance (Ord a) => Intersectable (Bag a) where    
    intersect = Bag.intersection


instance (Eq a, Ord a) => Intersectable [a] where    
    intersect = List.intersect


-------------------------------------------------------------------------------            
-- * Containment instances
-------------------------------------------------------------------------------            
instance (Ord a) => Containment (Bag a) where
    isSubset proper candidate source 
        = ifelse proper 
            (Bag.isProperSubsetOf candidate source) 
            (Bag.isSubsetOf candidate source)
    
instance (Eq a, Ord a) => Containment [a] where        
    isSubset proper candidate source  
        = test (Set.fromList candidate) (Set.fromList source)
            where test = ifelse proper Set.isProperSubsetOf Set.isSubsetOf 

instance Ord a => Containment (Set' a) where
    isSubset proper candidate source 
        = ifelse proper  
            (Set.isProperSubsetOf c' s')  
            (Set.isSubsetOf c' s')  
                where (c', s') = (candidate, source)
                        
