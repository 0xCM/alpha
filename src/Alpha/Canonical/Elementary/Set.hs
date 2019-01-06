-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Elementary.Set
(
    module X,
    SetBuilder(..),
    Subset(..),    
    Set(..),
    FneSet(..),
    emptyset, 
    finiteSet,
    fneset,

)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Common
import Alpha.Canonical.Elementary.Queries as X

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence
import qualified Numeric.Interval as Interval
         
data Construcability a =
      Constructive
    | Unconstructive

data SetDescription a 
    = SetDescription (Cardinality a, Discretion a, Habitation a, Construcability a)

data Set a
    = EmptySet
    | FiniteSet (Set' a) 
    | InfiniteSet
    deriving(Generic, Data, Typeable, Eq, Ord)
     

type instance Individual (Set a) = a

class Subset a b where
    subset::Set b -> Set a

class (a ~ Individual b) =>  SetBuilder b a where
    set::b -> Set a

instance (Ord a) => SetBuilder [a] a where
    set src = ifelse (List.null src) EmptySet (FiniteSet (fromList src))

-- | Represents a finite set that contains at least one element
newtype FneSet a = FneSet (Set' a)    
    deriving (Eq,Ord,Generic)

instance Newtype(FneSet a)    

type instance Individual (FneSet a) = a
    
-- | Constructs a representative for the empty set relative to the parameter 'a'
emptyset::Set a
emptyset = EmptySet

finiteSet::(Ord a) => [a] -> Set a
finiteSet = FiniteSet . fromList

powerset::Ord a => Set a -> Set(Set a)
powerset (FiniteSet src) = Set.map FiniteSet (powerset' src) |> FiniteSet
powerset _ = EmptySet

fneset::(Ord a) => NonEmpty a -> FneSet a
fneset (a :| xs) = FneSet (Set.fromList(a : xs))

formatset::(Formattable m) => [m] -> Text  
formatset s = fence LBrace RBrace (format elements) where
    elements =  weave Comma (format <$> s)

instance Ord a => IsList (Set a) where
    type Item (Set a) = a
    toList (FiniteSet s) = toList s
    toList _ = []

    fromList ([]) = EmptySet
    fromList (xs) = FiniteSet (fromList xs)

instance Ord a => Associated (Set a) where
    associates (FiniteSet s) = toList s
    associates (_) = []
    
instance Ord a => Discrete (Set a) where
    individuals = associates

instance Ord a => Finite (Set a) where
    count = fromIntegral . List.length . associates
        
instance (Ord a) => Unionizable (Set a) where
    union (FiniteSet x) (FiniteSet y )  =  FiniteSet $ Set.union x y    
    union EmptySet EmptySet = EmptySet
    union  (FiniteSet x) (EmptySet) = FiniteSet x
    union (EmptySet)  (FiniteSet y) = FiniteSet y
    union InfiniteSet  _ = undefined
    union _  InfiniteSet = undefined
    
instance Universal (Set a) where
    all pred (FiniteSet s) = s |> Set.toList |> List.all pred
    all pred (EmptySet) = True
    all pred (InfiniteSet) = undefined
    
instance Existential (Set a) where
    any pred (FiniteSet s) = s |> Set.toList |> List.any pred
    any pred (EmptySet) = False    
    any pred (InfiniteSet) = undefined

instance Ord a => Containment (Set a) where
    isSubset proper (FiniteSet candidate) (FiniteSet source) 
        = ifelse proper  
            (Set.isProperSubsetOf c' s')  
            (Set.isSubsetOf c' s')  where (c', s') = (candidate, source)
    isSubset proper (EmptySet) _ = True
    isSubset proper _ EmptySet = False
    isSubset proper (InfiniteSet) _ = undefined

instance Ord a => Differential (Set a) where
    diff (FiniteSet x) (FiniteSet y) = FiniteSet $ Set.difference x y
    
instance Ord a => Intersectable (Set a) where
    intersect (FiniteSet x) (FiniteSet y) = FiniteSet $ Set.intersection x y 
    
instance Formattable TyConInfo where
    format (TyConInfo (_,mod,ctor)) = mod <> fence LParen RParen ctor 

instance Show TyConInfo where
    show = string . format

instance (Formattable a, Ord a) => Formattable (Set a) where
    format  EmptySet =  LBrace <> RBrace
    format  InfiniteSet =  LBrace <> RBrace
    format (FiniteSet l) = formatset (toList l) where

        
instance (Ord a, Ord b) => Mappable(Set a) a b where
    type Mapped (Set a) a b = Set b
    map f (FiniteSet s) = FiniteSet $ Set.map f s
    map f (EmptySet) = EmptySet
    map f (InfiniteSet) = InfiniteSet

instance (Formattable a, Ord a) => Show (Set a) where
    show = Text.unpack . format

instance Ord a => Vacuous (Set a) where
    empty = EmptySet
    null (EmptySet) = True
    null _ = False
        
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

instance SetBuilder Int8 Int8 where
    set _ = set integers
instance SetBuilder Int16 Int16 where
    set _ = set integers
instance SetBuilder Int32 Int32 where
    set _ = set integers
instance SetBuilder Int64 Int64 where
    set _ = set integers
instance SetBuilder Word8 Word8 where
    set _ = set integers
instance SetBuilder Word16 Word16 where
    set _ = set integers
instance SetBuilder Word32 Word32 where
    set _ = set integers
instance SetBuilder Word64 Word64 where
    set _ = set integers

instance (Ord a) => Queryable (Set a) where
    filter p (FiniteSet s) = s |> toList |> List.filter p
    filter p _ = []        

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
    

instance Ord a => Differential (FneSet a) where
    diff (FneSet x) (FneSet y) = FneSet $ Set.difference x y
    
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
    format (FneSet l) = formatset (toList l) where

instance (Formattable a, Ord a) => Show (FneSet a) where
    show = Text.unpack . format
    

