-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Root
(
    module X,
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
    Formattable(..),
    Labeled(..),
    Cardinality(..),   
    Discretion(..),
    Habitation(..),
    enumValues,
    typeSymbol,
    typeSymbols,
    facetVal,
    mapi,
    ifelse, 
    clone,
    reduce,
    associate, 
    associated,
    (<|),(|>), 

) where
import Alpha.Base as X
import Alpha.Canonical.Common.Synonyms
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Numeric.Interval as Interval
import qualified Data.Map as Map
    


-- | Characterizes a value that can be rendered in human-readable form
class Formattable a where
    format ::a -> Text            

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
class Labeled a where
    type Label a    

    -- | Write a label to a value
    label::Label a -> a -> a

    -- | Read a label from a value
    getLabel::a -> Label a
    
        
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
class (Sourced a, Targeted b) => Connective a b where 
    -- | The reification type
    type Connection a b

    -- | Establishes a connection from a source to a target
    connect::a -> b -> Connection s t        

class Mappable c a b where    
    type Mapped c a b
    map::(a -> b) -> c -> Mapped c a b
    
class Weave g t where
    type Woven g t
    type Woven g t = t

    -- Weaves a grain 'g' with a target 't' to produce a 'Woven g t' value
    weave::g -> t -> Woven g t        
    
instance Weave Char Text where
    type Woven Char Text = Text
    weave = Text.intersperse
            
instance Weave g [g] where
    weave = List.intersperse        

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

-- | Specifies whether a type/value is discrete
data Discretion a =
      Discrete
    | Indiscrete

-- | Specifies whether a type/value is empty
data Habitation a =
      Inhabited
    | Uninhabited    


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
    classify::(a -> c) -> [a] -> [(c,a)]
    
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


mapi::(Integral i) => ((i,a) -> b) -> [a] -> [b]
mapi f l = f <$> z where 
    idx = [0..upper]
    upper  = sub'  (fromIntegral $ List.length l) 1
    z = List.zip idx l

-- | If the first input value is true, returns the 2nd input value,
-- otherwise, returns the third input value
ifelse::Bool -> a -> a -> a
ifelse x aye no = case x of
            True -> aye
            _ -> no

-- | The forward pipe operator
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>

-- | The backward pipe operator
(<|) :: (a -> b) -> a -> b
f <| x = f x
infixr 0 <|

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
