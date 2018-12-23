-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common
(
    module X,
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
    Variance(..),
    Variant(..),
    Specifiable(..),
    Formattable(..),
    Componentized(..),
    Concatenated(..),
    Concatenable(..), 
    Appended(..), 
    Appendable(..),
    Weavable(..),
    text,
    firstValue, 
    lastValue,
    enumValues,
    typeSymbol,
    typeSymbols,
    facetVal,
    mapi,
    ifelse, 
    (<|),(|>), 
)
where
import Alpha.Base as X
import Alpha.Native as X
import qualified Data.List as List
import qualified Data.MultiSet as Bag
import qualified Data.Text as Text

type family Concatenated a b

type instance Concatenated [a] [a] = [a]

-- | Characterizes a pair whose terms can be related via an append operation
class Concatenable a b where
    concat::a -> b -> Concatenated a b

    (++)::a -> b -> Concatenated a b
    (++) x y = concat x y
    infixr 5 ++

class Weavable g t where
    type Woven g t
    type Woven g t = t

    -- Weaves a grain 'g' with a target 't' to produce a 'Woven g t' value
    weave::g -> t -> Woven g t        
    
-- | Defines a family of type-level functions with the intent
-- of projecting nested a sequence of elements to a (relatively)
-- non-nested sequence of elements. An instance need not be
-- Element-invariant
type family Appended a

-- | A list of element lists is appended to produce a list of elements
type instance Appended [[a]] = [a]

-- | A list of elements is appended to produce a single element
type instance Appended [a] = a

-- Classifies a type that can be transformed into an 'Appended' value
class Appendable a where
    append::a -> Appended a

-- Synonym for combined Ord and Enum constraints
type OrdEnum a = (Enum a, Ord a)    

-- | Classifies types with which a sign cannot be associated
class Unsignable a where
    
-- / Characterizes a type with which a strictly monotonic sequence 
-- of ascending values is associated
class Successive a where
    next::a -> Maybe a

-- / Characterizes a type from which a sequence of components can be extracted
class Componentized a where
    type Component a
    components::a -> [Component a]    

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

-- | Characterizes a value that can be rendered in human-readable form
class Formattable a where
    format ::a -> Text

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

-- | Specifies whether a construct is covariant or contravariant    
data Variance = Contravariant | Covariant
    deriving (Eq, Enum, Ord)

instance Formattable Variance where
    format (Covariant) = "*"
    format (Contravariant) = "^"
    
-- | Advertises the variance of a construct
class Variant a where
    variance::a -> Variance    

-- | Captures the specification pattern
class Specifiable a where
    type Specified a
    specify::a -> Specified a

-- | Converts a showable to text    
text::(Show s) => s -> Text
text = Text.pack . show

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

facetVal::(Faceted f v) => v -> FacetValue f v
facetVal val = FacetValue val    

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

instance Faceted "length" Int where

instance Formattable Text where
    format s = s         
instance Formattable Char where
    format = Text.singleton 
instance Formattable Natural where
    format = text
instance Formattable Int where
    format = text
instance Formattable Word where
    format = text
instance Formattable Integer where
    format = text
instance (Show a) => Formattable (Ratio a) where
    format = text
instance Formattable Word8 where
    format = text
instance Formattable Word16 where
    format = text
instance Formattable Word32 where
    format = text
instance Formattable Word64 where
    format = text
instance Formattable Int8 where
    format = text
instance Formattable Int16 where
    format = text
instance Formattable Int32 where
    format = text
instance Formattable Int64 where
    format = text
instance Formattable Double where
    format = text
instance Formattable Float where
    format = text
instance Formattable CDouble where
    format = text
instance Formattable CFloat where
    format = text

instance (Formattable a) => Formattable [a] where
    format x = x |> (<$>) format |> Text.concat
            
    
instance Weavable Char Text where
    type Woven Char Text = Text
    weave = Text.intersperse
            
instance Weavable g [g] where
    weave = List.intersperse

    
    