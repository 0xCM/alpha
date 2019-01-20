-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Individual
(
    Individual(..),
    Associated(..),
    Discrete(..),
    Finite(..),
    Componentized(..),
    Universal(..),
    Existential(..),
    Constructive(..),
    Construction(..),
    Initializing(..),
    Terminating(..),
    Degenerate(..),
    IndexedMapping(..)

)
where
import Alpha.Canonical.Common.Root
import qualified Data.List as List
import qualified Data.MultiSet as Bag
import qualified Data.List as List
import qualified Data.Set as Set

-- | Relates the type of a part/singleton to the type of a whole
type family Individual a

type instance Individual [a] = a
type instance Individual (Bag a) = a
type instance Individual (Map a b) = (a,b)
type instance Individual (Stream a) = a
type instance Individual (NonEmpty a) = a
type instance Individual (Seq a) = a
type instance Individual (Vector a) = a
--type instance Individual Integer = Integer
--type instance Individual Int = Int
-- type instance Individual Int8 = Int8
-- type instance Individual Int16 = Int16
-- type instance Individual Int32 = Int32
-- type instance Individual Int64 = Int64
-- type instance Individual (Ratio a) = Ratio a
-- type instance Individual Natural = Natural
-- type instance Individual Word = Word
-- type instance Individual Word8 = Word8
-- type instance Individual Word16 = Word16
-- type instance Individual Word32 = Word32
-- type instance Individual Word64 = Word64
--type instance Individual Bool = Bool

-- | Characterizes a type that is comprised of individuals or
-- can be discretized as such
class Discrete a where
    individuals::a -> [Individual a]

-- | Characterizes types inhabited by a finite set of
-- values and for which a count is determined
class (Discrete a) => Finite a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n
    count a = individuals a |> List.length |> fromIntegral
        
-- | Characterizes a type that defines an association among a 
-- collection of individuals
class Associated a where
    -- | Specifies the members that participate in the association
    associates::a -> [Individual a]    

-- / Characterizes a type that owns a collection of individuals
class Componentized a where

    -- | Extracts the components that are owned by the whole
    components::a -> [Individual a]    

-- | Characterizes types for which existential questions may posed
-- regarding element containment
class Existential a where    
    -- Determines whether any element exists that satisfies a given predicate
    any::(Individual a -> Bool) -> a -> Bool

    -- Determines whether an exlement exists via an equality predicate
    exists::(Eq (Individual a)) => Individual a -> a -> Bool
    exists = any . (==) 

class Universal c where
    all::(Individual c -> Bool) -> c -> Bool
    
-- | Characterizes types from which an 'Individual' may be constructed    
class Constructive a where
    construct::a -> Individual a

-- | Characterizes a type for which a "first" element is defined
class Initializing a where
    first::a -> Individual a

-- | Characterizes a type for which a "last" element is defined
class Terminating a where
    last::a -> Individual a

-- | Characterizes a type that supports a notion of degenerecy,
-- e. g., an interval whose min/max values are identical
class Degenerate a where
    degenerate::a -> Bool    

-- | Provides concrete evidence of an existential    
newtype Construction a = Construction (Individual a)

type instance Individual (Construction a) = Individual a

class IndexedMapping a b where
    mapi::((Int, Individual a) -> Individual b) -> a -> b    
    
instance IndexedMapping [a] [b] where
    mapi f l = f <$> z where 
        idx = [0..upper]
        upper  = sub'  (fromIntegral $ List.length l) 1
        z = List.zip idx l

instance Constructive (Construction a) where
    construct (Construction x) = x
            
instance Universal [a] where
    all = List.all
                
instance Existential [a] where
    any = List.any

instance Initializing [a] where
    first = List.head

instance Terminating [a] where
    last = List.head . List.reverse