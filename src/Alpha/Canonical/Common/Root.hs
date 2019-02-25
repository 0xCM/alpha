-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Root
(
    module X,
    Descriptor(..),
    Dimensional(..),
    Weave(..),
    Variance(..),
    Variant(..),
    Faceted(..),
    FacetValue(..), 
    Reifiable(..),
    Chunkable(..),
    Classifiable(..),
    Specifiable(..),
    Mappable(..),  
    Sourced(..),
    Targeted(..),  
    Connective(..),
    Packable(..),
    Labeled(..),
    Discretion(..),
    Generative(..),
    Nullity(..),
    Cell(..),
    Cellular(..),
    Computable(..),    
    Complementable(..),
    FloatInfo(..),
    Partitionable(..),
    SymbolicName(..),
    Simplifiable(..),
    JoinableMeet(..),
    Collapsed(..), 
    Collapsible(..),
    Space(..),
    Point(..),
    Expansive(..),
    Evaluation(..),
    Evaluated(..),
    Mor(..),
    Structure(..),
    StructureN(..),
    floatinfo,
    enumValues,
    typeSymbol,
    typeSymbols,
    facetVal,
    ifelse, 
    clone,
    range,
    reduce,
    associate, 
    associated,
    (<|),(|>), 
    mapi,
    zip,
    pairzip,
    mix,
    intermix,
    segment,
    exclude,
    valOrDefault

) where
import Alpha.Base as X
import Alpha.Canonical.Common.Synonyms as X
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.MultiSet as Bag
import qualified Data.Set as Set
import qualified Data.Stream.Infinite as Stream
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence
import qualified Data.Stream.Infinite as Stream
import qualified Data.Vector as Vector
import qualified Numeric.Interval as Interval
import qualified Data.Text as Text
import qualified Data.ByteString as EG
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Tree as Tree

-- | Specifies the type of a point relative to a set/space
type family Point d

type family Sig (i::Nat) = r | r -> i
type instance Sig 1 = Type -> Constraint
type instance Sig 2 = Type -> Type -> Constraint
type instance Sig 3 = Type -> Type -> Type -> Constraint

type family SigN (i::Nat) (n::Nat) = r | r -> i
type instance SigN 1 n = Nat -> Type -> Constraint
type instance SigN 2 n = Nat -> Type -> Type -> Constraint
type instance SigN 3 n = Nat -> Type -> Type -> Type -> Constraint
   
class Structure i (c::Sig i) where

class StructureN i (c::SigN i n) where
    
-- Defines a family of structure-preserving functions (for groups, modules, etc)
-- See https://en.wikipedia.org/wiki/Morphism
type family Mor (s::Constraint) a b :: Type
type instance Mor (Structure n s) a b = a -> b

newtype Evaluation a b = Evaluation (a, b)
    deriving (Eq,Ord,Generic,Data,Typeable)
instance Newtype (Evaluation a b)    

class Evaluated a where
    type Input a
    type Output a

    input::a -> Input a

    output::a -> Output a

instance Evaluated (Evaluation a b)  where
    type Input (Evaluation a b) = a
    type Output (Evaluation a b) = b

    input (Evaluation (a,_)) = a
    output (Evaluation (_,b)) = b

class Space a where
    type PointSource a
    
    -- | Constructs a point in the product space
    point::PointSource a -> Point a

class JoinableMeet a where
    -- | The join
    (\/)::a -> a -> a
    infixr 5 \/

    -- | The meet
    (/\)::a -> a -> a
    infixr 6 /\
    
class Duality a where
    type Dual a

    -- | Calculates a dual value
    dual::a -> Dual a

    -- | Calculates a dual of a dual which is required 
    -- to match the primal value
    codual::Dual a -> a

-- | Characterizes a type that can be reduced to a cononical, but equivalent, form
class Simplifiable a where
    
    -- Idempotent operation yields the canonical "simplified" value of a given type
    simplify::a -> a

class (KnownSymbol s) => SymbolicName s a where
    symname::Text
    symname = Text.pack (symbol @s)

class Partitionable a where
    type PartitionSegment a
    type PartitionCriteria a

    partition::PartitionCriteria a -> a -> [PartitionSegment a]
    
-- | Represents a located value
newtype Cell l v = Cell (l, v)
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 

-- | Characterizes a located value
class Cellular a where
    type Location a
    type Value a
    
    -- | Constructs a cell given a location and value
    cell::Location a -> Value a -> Cell (Location a) (Value a)

    -- | Extracts a cell's value
    value::Cell (Location a) (Value a) -> Value a
    
    -- | Extracts a cell's location
    location::Cell (Location a) (Value a) -> Location a
    
-- Characterizes a type with wich a description may be associated
class Descriptor a b where
    describe::a -> b

-- | Characterizes a pair of types for which transformations are defined 
-- for respectively "packing"  and "unpacking" type values
class Packable a b where
    -- Encodes an a-value to a b-value
    pack::a -> b
    -- Restores an a-value from a b-value
    unpack::b -> a    

-- | Characterizes a type for which a notion of dimensionality 
-- can be defined, e.g., an array, matrix or more generally a tensor
class Dimensional a where
    type Dimension a
    dimension::a -> Dimension a

-- | Characterizes a type with which defines an intrinsic label
class Labeled a l where
    
    -- | Write a label to a value
    label::l -> a -> a

    -- | Read a label from a value
    getLabel::a -> l
    
-- | Characterizes a type whose values can be embedded into another type's values
-- via a suitably-defined inclusion map
class Inclusional a b where
    include::a -> b -> b
        
-- | Characterizes a type with which an origin/initial object is related
class Sourced a where
    type Source a
    source::a -> Source a

-- | Characterizes a type with which a destination/final object is related
class Targeted a where
    type Target a
    target::a -> Target a
        
-- } Characterizes a type that controls the reification of a directed relationship    
-- from a source to a target
class Connective p s t where 
    -- | The reification type
    type Connection p s t

    -- | Establishes a connection from a source to a target
    -- and bundles said connection with properties/attributes
    -- that further characterize the association
    connect::p -> s -> t -> Connection p s t        
    
class Mappable c a b where    
    type Mapped c a b
    map::(a -> b) -> c -> Mapped c a b
    
class Weave g t where
    type Woven g t
    type Woven g t = t

    -- Weaves a grain 'g' with a target 't' to produce a 'Woven g t' value
    weave::g -> t -> Woven g t        
        
-- | Characterizes a deferred computation or a computation
-- specification    
class Computable a where

    -- | The type of computed value
    type Computed a

    -- | Effects the computation
    compute::a -> Computed a


-- | Specifies whether a type/value is discrete
data Discretion a =
      Discrete
    | Indiscrete
    deriving (Eq, Ord, Generic, Data, Typeable, Enum)

-- / Characterizes a type for which a canonical and unique Vacatable/void/empty
-- value exists
class Nullity a where

    -- | Exhibits the canonical empty value
    empty::a

    -- | Determines whether a given value is the canonical
    -- 'empty' value
    null::a -> Bool

-- | Specifies whether a construct is covariant or contravariant    
data Variance = Contravariant | Covariant
    deriving (Eq, Enum, Ord)
    
-- | Advertises the variance of a construct
class Variant a where
    variance::a -> Variance    
    
class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  Text.pack $ symstr @f 
    
data FacetValue f v = FacetValue v        
    
instance Faceted "length" Int where            

-- Captures the assertion that values of a type a can be categorized by
-- values of an enumerable type c
class (Enum c) => Classifiable a c where
    classification::(a -> c) -> [a] -> [(c,a)]
    
-- Characterizes a family of singleton types 'a' for which the type's single inhabitant
-- is reifiable
class Reifiable (a::k) r where
    reify::r 
        
-- | Characterizes a finite container or other type that contains elements
-- that can be separated into groups of possibly different sizes
class Chunkable a where
    chunk::Int -> a -> [a]

-- | Captures the specification pattern
class Specifiable a where
    type Specified a
    specify::a -> Specified a

-- | Characterizes a type that induces the construction of 
-- colleciton of values
class Generative a where
    type Generated a
    generate::a -> [Generated a]
    
-- | Characterizes a type that supports the notion of 'complement'
-- in the following sense: complement . complement = id
class Complementable a where
    complement::a -> a    


-- | Defines a family of type-level functions with the intent
-- of projecting nested a sequence of elements to a (relatively)
-- non-nested sequence of elements. An instance need not be
-- Element-invariant
type family Collapsed a
type instance Collapsed [[a]] = [a]
type instance Collapsed [a] = a
type instance Collapsed (Tree a) = [a]

-- Classifies a type that can be transformed into an 'Appended' value
class Collapsible a where
    collapse::a -> Collapsed a
    
-- | Describes floating-point characteristics that are speific to the
-- executing machine that includes 
-- 1. floating point radix ,
-- 2. the number of digits in the significand (relative to the radix)
-- 3. the min/max value of the floating point exponent
-- For a typical x86-64bit machine, the respective values are 2, 53 and (-1021,1024)
data FloatInfo a = FloatInfo Integer Int (Int,Int)
    deriving(Eq,Ord,Generic,Data,Typeable,Show)

-- | Characterizes a type that can be expressed as a sequence of terms
-- of a specified type    
class Expansive a where
    type Expanded a

    expand::a -> [Expanded a]

-- | The forward pipe operator
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>

-- | The backward pipe operator
(<|) :: (a -> b) -> a -> b
f <| x = f x
infixr 0 <|

floatinfo:: forall a. RealFloat a => FloatInfo a
floatinfo = FloatInfo (floatRadix zed ) (floatDigits zed) (floatRange zed)

facetVal::(Faceted f v) => v -> FacetValue f v
facetVal val = FacetValue val    
    
firstValue::(Enum e) => e
firstValue = toEnum 0

-- | Retrieves the last value of a bounded enum
lastValue::(Bounded e, Enum e, Eq e) => e
lastValue = List.head (List.reverse enumValues)

-- | Retrieves all values of a bounded enum
enumValues::(Bounded e, Enum e) => [e]
enumValues = enumFrom firstValue

-- | Forms a 'SomeSymbol' value from a 'String'
typeSymbol::String -> SomeSymbol
typeSymbol = someSymbolVal

-- | Forms a list of 'SomeSymbol' values from a list of'String'
typeSymbols::[String] ->[SomeSymbol]
typeSymbols s = typeSymbol <$> s

-- | Constructs a contiguous sequence of values inclusively between
-- a spcified min and max value
range::(Ord i, Enum i) => (i,i) -> [i]
range (min,max) = [min..max]


-- | If the first input value is true, returns the 2nd input value,
-- otherwise, returns the third input value
ifelse::Bool -> a -> a -> a
ifelse x aye no = case x of
            True -> aye
            _ -> no

reduce::a -> O2 a -> [a] -> a
reduce id op (a:b:tail) =  op (op a b)  (reduce id op tail)
reduce id op (a:[]) = a
reduce id op [] = id

clone::(Integral n) => n -> a -> [a]
clone n a = List.replicate (fromIntegral n) a

-- | Produces an associative array for a list of key-value pairs
associate::Ord k => [(k,v)] -> Map k v
associate = Map.fromList

-- | Extracts the indexed values from a map
associated::Ord k => Map k v -> [v]
associated m = snd <$> (toList m)

zip::(a -> b -> c) -> [a] -> [b] -> [c]
zip = List.zipWith            

pairzip::[a] -> [b] -> [(a,b)]
pairzip = List.zip

mapi::Eq a => ((Int, a) -> b) -> [a] -> [b]
mapi f l = f <$> z where 
    idx = [0..upper]
    upper  = sub'  (fromIntegral $ List.length l) 1
    z = pairzip idx l

mix::(a -> b -> c) -> ([a],[b]) -> [c]
mix f (a,b) = (\(x,y) -> f x y) <$> pairzip a b

intermix::[a] -> [a] -> [a]
intermix x y = join <| do
    i <- [0 .. n]
    [[x List.!! i, y List.!! i] ] where
        len = min' (List.length x) (List.length y)  
        n = sub' len 1  

segment::Int -> [a] -> [[a]]
segment width = List.takeWhile (not' . List.null) . fmap (List.take width) . List.iterate (List.drop width)    

-- | Excludes a specified subset of list elements from the result list
exclude::(Eq a) => [a] -> [a] -> [a] 
exclude exclusions source = source |>  List.filter ( `List.notElem` exclusions) 

valOrDefault::Maybe a -> a -> a
valOrDefault Nothing def = def
valOrDefault (Just val) _ = val

-- * Mappable instances
-------------------------------------------------------------------------------
instance Mappable (Seq a) a b where
    type Mapped (Seq a) a b = Seq b
    map = fmap

instance Mappable [a] a b where
    type Mapped [a] a b = [b]
    map = fmap

instance Mappable (NonEmpty a) a b where
    type Mapped (NonEmpty a) a b = NonEmpty b
    map = NonEmpty.map    
        
instance OrdPair a b => Mappable(BalancedSet a) a b where
    type Mapped (BalancedSet a) a b = BalancedSet b
    map f s = Set.map f s
    
-- *Weavable instances
-------------------------------------------------------------------------------    
instance Weave Char Text where
    type Woven Char Text = Text
    weave = Text.intersperse
            
instance Weave g [g] where
    weave = List.intersperse        

instance Weave g (Stream g) where
    weave = Stream.intersperse

instance Weave a (NonEmpty a) where
    weave = NonEmpty.intersperse        

-- * Nullity instances
-------------------------------------------------------------------------------    
instance Eq a => Nullity (Interval a) where
    empty = Interval.empty
    null = Interval.null

instance Nullity [a] where
    empty = []
    null = List.null
    
instance Nullity (Map k v) where
    empty = Map.empty
    null = Map.null
        
instance Nullity (Bag a) where
    empty = Bag.empty
    null = Bag.null

instance Nullity (Vector a) where
    empty = Vector.empty
    null = Vector.null

instance Ord a => Nullity (BalancedSet a) where
    empty = Set.empty
    null = Set.null
    
-- * Appendable instances
-------------------------------------------------------------------------------    
instance Collapsible (Tree a) where
    collapse = Tree.flatten

instance Collapsible [[a]] where
    collapse = List.concat    
    
instance Collapsible [Text] where    
    collapse = Text.concat        

