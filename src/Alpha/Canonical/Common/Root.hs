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
    Componentized(..),
    Faceted(..),
    FacetValue(..), 
    Reifiable(..),
    Chunkable(..),
    Classifiable(..),
    Specifiable(..),
    Mappable(..),    
    enumValues,
    typeSymbol,
    typeSymbols,
    facetVal,
    mapi,
    ifelse, 
    clone,

    (<|),(|>), 

) where
import Alpha.Base as X
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Numeric.Interval as Interval
import qualified Data.Map as Map

    
-- Characterizes a type for which a notion of dimensionality 
-- can be defined, e.g., an array, matrix or more generally a tensor
class Dimensional a where
    type Dimension a
    dimension::a -> Dimension a

-- / Characterizes a type from which a sequence of components can be extracted
class Componentized a where
    type Component a
    components::a -> [Component a]    
    
    
    
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

clone::(Integral n) => n -> a -> [a]
clone n a = List.replicate (fromIntegral n) a
