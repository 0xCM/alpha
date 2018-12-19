-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common
(
    Successive(..),
    Antecedent(..),
    Absolute(..),
    BoundedIntegral(..),
    OrdEnum(..),
    Unsignable(..),
    Reifiable(..),
    Chunkable(..),
    Classifiable(..),
    Faceted(..),
    FacetValue(..), 
    firstValue, 
    lastValue,
    enumValues,
    typeSymbol,
    typeSymbols,
    facetVal,

)
where
import Alpha.Base
import Alpha.Native
import qualified Data.List as List
import qualified Data.MultiSet as Bag
import qualified Data.Text as Text

-- Synonym for combined Ord and Enum constraints
type OrdEnum a = (Enum a, Ord a)    

-- | Classifies types with which a sign cannot be associated
class Unsignable a where
    
-- / Characterizes a type with which a strictly monotonic sequence 
-- of ascending values is associated
class Successive a where
    next::a -> Maybe a

-- / Characterizes a type with which a strictly monotonic sequence 
-- of descending values is associated
class Antecedent a where    
    prior::a -> Maybe a

class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  Text.pack $ symstr @f 
    
data FacetValue f v = FacetValue v        

-- Characterizes types that support a notion of absolute/unsigned value
class Absolute a where
    abs::a -> a

class (Bounded a, Integral a) => BoundedIntegral a where

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
                            
-- | Retrieves the first value of an enum
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


instance BoundedIntegral Int
instance BoundedIntegral Int8
instance BoundedIntegral Int16
instance BoundedIntegral Int32
instance BoundedIntegral Int64
instance BoundedIntegral Word
instance BoundedIntegral Word8
instance BoundedIntegral Word16
instance BoundedIntegral Word32
instance BoundedIntegral Word64        
            
instance Unsignable Natural
instance Unsignable Word
instance Unsignable Word8
instance Unsignable Word16
instance Unsignable Word32
instance Unsignable Word64


facetVal::(Faceted f v) => v -> FacetValue f v
facetVal val = FacetValue val    

instance Faceted "length" Int where
