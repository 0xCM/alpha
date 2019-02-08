-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Individual
(
    Individual(..),
    Reduced(..),
    FinitelyConstructible(..),
    FinitelyCountable(..),
    Discrete(..),
    Discretizable(..),
    Universal(..),
    Existential(..),
    Singletary(..),
    Singleton(..),
    Construction(..),
    HasFirst(..),
    HasLast(..),
    Degenerate(..),
    Endpointed(..),
    Vectored(..),
    Groupable(..),
    Grouping(..),    
    Filter(..),
    Queryable(..),
    Transposable(..),
    Indexable(..),
    Container(..),
    Selector(..),
    Iterable(..),    
)
where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Synonyms
import qualified Data.List as List
import qualified Data.MultiSet as Bag
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Stream.Infinite as Stream
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map

-- | Defines the canonical constituent type of an aggregant
type family Individual a

type instance Individual [a] = a
type instance Individual (Bag a) = a
type instance Individual (Map a b) = (a,b)
type instance Individual (Stream a) = a
type instance Individual (NonEmpty a) = a
type instance Individual (Seq a) = a
type instance Individual (Vector a) = a

type family Reduced a
type instance Reduced [a] = a


class Container a where
    contains::Bool -> a -> a -> Bool
    content::a -> [Individual a]

class Indexable a where
    type Indexer a
    type Indexer a = Int
    
    idx::a -> Indexer a -> Individual a
    idx = (!!)
    {-# INLINE idx #-}

    (!!)::a -> Indexer a -> Individual a
    (!!) = idx
    {-# INLINE (!!) #-}
    infixr 9 !!

type Grouping a = [Individual a]    

class Filter a where
    allow::Individual a -> Bool

class Transposable a where
    type Transposed a
    transpose::a -> Transposed a
    
class Queryable a where

    -- | Excludes elements that don't satisfy a predicate
    filter::P1(Individual a) -> a -> [Individual a]

    -- | Selects a single element, if any, that satisfies a predicate
    single::P1(Individual a) -> a -> Maybe (Individual a) 
    single p src = ifelse (List.length filtered == 1) (Just $ List.head filtered) Nothing  
        where
            filtered = filter p src
                
class Selector q a where
    select::q -> a -> a

class Groupable a where
    groups::(Individual a -> Individual a -> Bool) -> a -> [Grouping a]

-- | Characterizes a type from which a vector of individuals can be constructed
class Vectored a where

    -- Constructs a vector from a source value
    vector::a -> Vector (Individual a)

-- | Characterizes a type that is comprised of constitents
class Discrete a where

    -- | Extracts the constituent values from a source aggregant
    individuals::a -> [Individual a]

-- | Characterizes a type that can be expressed as a collection of 
-- constituent values
class Discretizable a where
    type Discretized a
    discretize::a -> [Discretized a]
        
-- | Characterizes a type that can be constructed from a finite
-- number of individuals
class FinitelyConstructible a where
    
    -- Constructs an aggregant from a list of supplied constituents
    finite::[Individual a] -> a
    
-- | Characterizes types inhabited by a finite number of
-- values for which a count can be determined
class FinitelyCountable a where
    -- | Counts the number of items within the purview of the subject
    count::Integral n => a -> n
   
-- | Characterizes a type that supports the evaluation of an 
-- existential predicate over its constituents
class Existential a where    
    -- Determines whether any element exists that satisfies a given predicate
    any::(Individual a -> Bool) -> a -> Bool

    -- Determines whether an exlement exists via an equality predicate
    exists::(Eq (Individual a)) => Individual a -> a -> Bool
    exists = any . (==) 

-- | Characterizes a type that supports the evaluation of a universal predicate 
-- over its constituents
class Universal c where
    all::(Individual c -> Bool) -> c -> Bool
    
-- | Characterizes types from which an 'Individual' may be constructed    
class Singletary a where
    individual::a -> Individual a

-- | Characterizes a type that can be constructed from exactly one individual
class Singleton a where
    singleton::Individual a -> a

-- | Characterizes a type for which a "first" element is defined
class HasFirst a where
    first::a -> Individual a

-- | Characterizes a type for which a "last" element is defined
class HasLast a where
    last::a -> Individual a

-- | Characterizes a type that supports a notion of degenerecy,
-- e. g., an interval whose min/max values are identical
class Degenerate a where
    degenerate::a -> Bool    

-- | Characterizes a type that has booth a "first" and "last" element
class (HasFirst a, HasLast a) => Endpointed a where    
    endpoints::a -> (Individual a, Individual a)
    endpoints a = (first a, last a)

-- | Characterizes a type over which function iterates may be computed
class Iterable a where
    iterate :: O1 (Individual a) -> (Individual a) -> a


-- | Provides concrete evidence of an existential    
newtype Construction a = Construction (Individual a)
type instance Individual (Construction a) = Individual a

instance Singletary (Construction a) where
    individual (Construction x) = x

instance FinitelyConstructible [a] where
    finite = id

instance FinitelyCountable [a] where
    count = fromIntegral . List.length


-------------------------------------------------------------------------------
-- * Vectored instances
-------------------------------------------------------------------------------
instance Vectored [a] where
    vector = Vector.fromList

-------------------------------------------------------------------------------
-- * Discrete instances
-------------------------------------------------------------------------------    
instance Discrete [a] where
    individuals = id
        
-------------------------------------------------------------------------------
-- * Universal instances
-------------------------------------------------------------------------------
instance Universal [a] where
    all = List.all
              
-------------------------------------------------------------------------------
-- * Existential instances
-------------------------------------------------------------------------------    
instance Existential [a] where
    any = List.any

instance HasFirst [a] where
    first = List.head

instance HasLast [a] where
    last = List.head . List.reverse

instance Endpointed [a]    

instance Groupable [a] where
    groups = List.groupBy

-- | Reserved names
class Membership a where    
class Componentized a where
    
    
-------------------------------------------------------------------------------            
-- * Singleton instances
-------------------------------------------------------------------------------        

instance Singleton [a] where
    singleton x = [x]

instance Ord k => Singleton (Map k v) where
    singleton (k,v) = fromList [(k,v)]
    
instance Singleton (Seq a) where
    singleton x = fromList [x]

instance Singleton (Vector a) where
    singleton a = Vector.fromList [a]

instance Singleton (Stream a) where
    singleton x = Stream.cycle (x :| [])
        
instance (Ord a) => Singleton (Bag a) where
    singleton a = Bag.fromList [a]
        
-- * Queryable instances
-------------------------------------------------------------------------------            
instance Queryable [a] where
    filter pred source = List.filter pred source
    
instance Queryable (Seq a) where
    filter pred source =  toList source |> List.filter pred

instance Queryable (Vector a) where
    filter pred source =  toList source |> filter pred

-- * Selector instances
-------------------------------------------------------------------------------            
instance Selector (P1 a) [a] where
    select pred source = List.filter pred source

-- *Transposable instances
-------------------------------------------------------------------------------        
instance Transposable [[a]] where
    type Transposed [[a]] = [[a]]
    transpose = List.transpose

-- * Indexable instances
-------------------------------------------------------------------------------            
instance Eq a => Indexable [a] where    
    idx = (List.!!)

instance Indexable (Seq a) where
    idx = Sequence.index
    
instance Indexable (Vector a) where
    idx vector i = vector Vector.! i

instance Ord k => Indexable (Map k v) where
    type Indexer (Map k v) = k
    idx map k = (k, map Map.! k)


