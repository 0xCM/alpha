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
    SetSpec(..),
    SetBuilder(..),
    Subset(..),
    DiscreteSubset(..),
    NonEmptySet(..),
    Cardinality(..),   
    Set(..),

    subset',
    emptyset, setrep, discrep, indiscrete, 

)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.SetConstraints as X
import Alpha.Canonical.Elementary.FiniteSet as X
import Alpha.Canonical.Elementary.Individual as X
import Alpha.Canonical.Elementary.InfiniteSet as X

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence


-- data SetRep a =
--     DiscreteSet
     
-- | Specifies the cardinality of a set and partitions the universe
-- of sets under consideration
-- See https://en.wikipedia.org/wiki/Cardinality
data Cardinality a =
    -- | There are no elements
    Zero
    -- | There is exactly one element
   | Singleton
   -- | A finite number of elements of count 2 or greater
   | Counted
   -- | A countably-infinite number of elements
   | Countable
   -- | An uncountable number of elements
   | Uncountable
   -- | An unknown number of elements
   | Uncounted
   deriving (Generic, Data, Typeable, Enum)

-- | Specifies whether a set is discrete
data SetDiscretion a =
      Discreete
    | Indiscrete

-- | Specifies whether a set is empty
data SetHabitation a =
      Inhabited
    | Uninhabited    

data SetProduction a =
      Constructible
    | Unconstructible  

data SetDescription a 
    = SetDescription (Cardinality a, SetDiscretion a, SetHabitation a, SetProduction a)

data Set a
    = Finite (FiniteSet a) 
    | Infinite (InfiniteSet a)
    deriving(Generic, Data, Typeable, Eq, Ord)

data SetSpec a 
    = UnconsRep
    -- | Identifies a representative for a discrete and countable set
    | DiscreteRep
    -- | Identifies the empty set
    | EmptyRep
    -- | Identifies a set that is non-discretizable and non-countable
    | IndiscreteRep   
    -- | Identifies a representative for which an explict set can be constructed
    | ConsRep     
        -- | Specifies a set with explicit content
    | UncountedSet [a]
      -- | Specifies a set with explicit finite content, if nonempty
    | CountedSet Int (FiniteSet a)
      -- | Specifies a set with explicit nonempty content
    | NeSet (NonEmpty a)
     -- | Specifies a finite non-empty set
    | FneSet (FiniteSet a)    
        deriving(Generic, Data, Typeable, Eq, Ord)
     
type instance Individual (Set a) = a
type instance Individual (SetSpec a) = a




-- | Constructs a representative for the empty set relative to the parameter 'a'
setrep::SetSpec a
setrep = UnconsRep

-- | Constructs the default set representative
emptyset::SetSpec a
emptyset = EmptyRep

-- | Constructs a finite, nonempty set
fneSet::(Ord a) => NonEmpty a -> SetSpec a
fneSet (a :| items) = FneSet $ fromList (a:items)

-- | Constructs a representative for a discrete set
discrep::(Discrete a) => SetSpec a
discrep = DiscreteRep

-- | Constructs a representative for a non-discretizable set
indiscrete::SetSpec a
indiscrete = IndiscreteRep

-- | Constructs an explicit finite set if source is nonempty, otherwise 
-- returns the empty set
-- finiteSpec::(Ord a) => [a] -> SetSpec a
-- finiteSpec items = 
--     if (List.null items) 
--         then EmptyRep 
--         else CountedSet 
--             (List.length items) 
--             (FiniteSet (fromList items))


countedset::(Ord a) => FiniteSet a -> SetSpec a
countedset (FiniteSet s) = 
    if Set.null s 
        then EmptyRep 
        else CountedSet (Set.size s) (FiniteSet s) 

neSet::(Ord a) => NonEmpty a -> SetSpec a
neSet (a :| items) = NeSet $ fromList (a:items)

isFneSet::SetSpec a -> Bool
isFneSet (FneSet _) = True
isFneSet _ = False

powerset::Ord a => FiniteSet a -> FiniteSet(FiniteSet a)
powerset (FiniteSet src) = Set.map FiniteSet (powerset' src) |> FiniteSet 

            
subset'::forall a b. Subset a b => SetSpec a
subset' = subset @a (UnconsRep @b)

-- | Characterizes a discrete set    
class Discrete a where
    elements::SetSpec a -> [Individual a]    

-- | Classifies a type whose values can be interpreted as a 
-- subset of another type's values
class Subset a b where
    subset::SetSpec b -> SetSpec a
    
-- | Classifies a type for which a discrete set of values can be interpreted as a 
-- subset of another type's values
class (Subset a b, Discrete a) => DiscreteSubset a b where
    subelements::SetSpec b -> [Individual a]
    subelements b = elements (subset @a b)
    
class SetSpecBuilder s where
    -- | Extracts the elements from a structure
    setspec::s ->  [Individual s] --SetSpec (Individual s)
    
class (a ~ Individual b) =>  SetBuilder b a where
    set::b -> Set a

instance Default (SetSpec a) where
    def = EmptyRep
        
-- instance Ord a => SetSpecBuilder [a] where
--     setspec items = 
--         if (List.null items) 
--         then EmptyRep 
--         else CountedSet count fs where
--             count = (fromIntegral $ List.length items)
--             fs = fromList items |> FiniteSet
    
instance Formattable TyConInfo where
    format (TyConInfo (_,mod,ctor)) = mod <> enclose LParen RParen ctor 

instance Show TyConInfo where
    show = string . format

instance (Formattable a, Ord a) => Formattable (Set a) where
    format (Finite l) = format l
    format (Infinite l) = format l

instance (Formattable a, Ord a) => Show (Set a) where
    show = Text.unpack . format
    
instance forall a. (Formattable a, Typeable a, Ord a) => Formattable (SetSpec a) where
    format x = fmt x where
        tyName = typerep @a |> show |> Text.pack            
        fmtU s = (List.take 10 s) |> (<$>) format |> List.intersperse Comma |> append |> enclose LBrace RBrace

        fmtC::Int -> [a] -> Text
        fmtC n s = format <$> s |> List.intersperse  Comma |> append |> enclose LBrace RBrace
    
        fmt (UnconsRep) =  rspaced tyName <> format (tycon @(UnconsRep::SetSpec a)) 
        fmt (DiscreteRep) = rspaced tyName <> format (tycon @(DiscreteRep::SetSpec a)) 
        fmt (EmptyRep) = rspaced tyName <>  format (tycon @(EmptyRep::SetSpec a)) 
        fmt (IndiscreteRep) = rspaced tyName <> format (tycon @(IndiscreteRep::SetSpec a)) 
        fmt (UncountedSet s) = fmtU s
        fmt (CountedSet n s) = fmtC n (list s)
        fmt (NeSet s) = (fmtU $ list s)
        fmt (FneSet (FiniteSet s)) = fmtC (length s) (list s)
                        
    
instance (Formattable a,Typeable a, Ord a) => Show (SetSpec a) where
    show = Text.unpack . format 
        
-- instance Ord a => IsList (SetSpec a) where
--     type Item (SetSpec a) = a
--     toList (UnconsRep) = []
--     toList (DiscreteRep) = []
--     toList (EmptyRep) = []
--     toList (IndiscreteRep) = []
--     toList (UncountedSet items) = toList items
--     toList (CountedSet _ items) = toList items
--     toList (NeSet items) = toList items
--     toList (FneSet items) = toList items

--     fromList [] = EmptyRep
--     fromList x = setspec x 

instance (Ord a) => Membership (SetSpec a) where
    -- | Identifies a representative for which an explict set can be constructed
    --members (ConsRep) =  members (construct::Set a)
    members (UncountedSet s) = s
    members (CountedSet _ (FiniteSet s)) = toList s
    members (NeSet s) = toList s
    members (FneSet s) = toList s  
    members(_) = []

-- instance (Ord a) =>  Unionizable (SetSpec a) where
--     union a b = setspec $ List.union (toList a) (toList b)    

--     unions::[SetSpec a] -> SetSpec a
--     unions sets = undefined --(unwrap <$> sets) |> Set.unions |> FiniteSet
        
instance Subset Int8 Int16 where
    subset _ = setrep 
instance Subset Int16 Int32 where
    subset _ = setrep 
instance Subset Int32 Int64 where
    subset _ = setrep 
instance Subset Int64 Integer where
    subset _ = setrep 
instance Subset Word8 Word16 where
    subset _ = setrep 
instance Subset Word16 Word32 where
    subset _ = setrep 
instance Subset Word32 Word64 where
    subset _ = setrep 
instance Subset Word64 Natural where
    subset _ = setrep 
instance Subset Natural Integer where
    subset _ = setrep 

instance (Ord a) => SetBuilder (UniTuple1 a) a where
    set (UniTuple1 a1) = finite [a1] |> Finite
instance (Ord a) => SetBuilder (UniTuple2 a) a where
    set (a1,a2) = finite [a1,a2] |> Finite
instance (Ord a) => SetBuilder (UniTuple3 a) a where
    set (a1,a2,a3) = finite [a1,a2,a3] |> Finite
instance (Ord a) => SetBuilder (UniTuple4 a) a  where
    set (a1,a2,a3,a4) = finite [a1,a2,a3,a4]  |> Finite
instance (Ord a) => SetBuilder (UniTuple5 a) a where
    set (a1,a2,a3,a4,a5) = finite [a1,a2,a3,a4,a5] |> Finite

integers'::(Bounded i, Integral i) => [i]    
integers' = [minBound .. maxBound]

instance SetBuilder Int8 Int8 where
    set _ = finite integers' |> Finite
instance SetBuilder Int16 Int16 where
    set _ = finite integers' |> Finite
instance SetBuilder Int32 Int32 where
    set _ = finite integers' |> Finite
instance SetBuilder Int64 Int64 where
    set _ = finite integers' |> Finite
instance SetBuilder Word8 Word8 where
    set _ = finite integers' |> Finite
instance SetBuilder Word16 Word16 where
    set _ = finite integers' |> Finite
instance SetBuilder Word32 Word32 where
    set _ = finite integers' |> Finite
instance SetBuilder Word64 Word64 where
    set _ = finite integers' |> Finite
    