module Alpha.Canonical.Common
(
    Enumerable(..),
    Absolute(..),
    BoundedIntegral(..),
    OrderedEnum(..),
    Unsignable(..),
    Reifiable(..),
    Cloneable(..),
    Chunkable(..),
    Classifiable(..),
    firstValue, 
    lastValue,
    enumValues,
    typeSymbol,
    typeSymbols,


)
where
import Alpha.Base
import Alpha.Native
import qualified Data.List as List


-- Synonym for combined Ord and Enum constraints
type OrderedEnum a = (Enum a, Ord a)    

-- Classifies types with which a sign cannot be associated
class Unsignable a where

class Enumerable a where
    next::a -> Maybe a
    prior::a -> Maybe a

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
        
-- Characterizes a value 'a' than can be duplicated according to a specification 'b'  
-- yielding a structure 'Cloned a b' that encloses the duplicates
class Cloneable a b where
    type Cloned a b
    
    clone::a -> b -> Cloned a b

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
