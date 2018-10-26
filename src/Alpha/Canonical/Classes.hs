-----------------------------------------------------------------------------
-- | Fundamental constraints
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Classes 
(
    Concatenable(..),
    Decomposable(..),
    ToString(..),
    Formattable(..),
    FromText(..),
    Counted(..),
    Jailbreak(..),
    Producer(..),
    Convertible(..),
    Indexed(..),
    Packable(..),
    Length(..),
    Proxy(..),
    ToLines(..),
    ToBool(..),
    Wrapped(..),
    Reifiable(..),
    Chunkable(..),
    Structured(..),
    Enumerable(..),
    Singletary(..),
    Existential(..),
    Faceted(..),
    Flow(..), Coflow(..),
    Reducible(..),
    Reversible(..)
)
where
import qualified Data.Text as T
import GHC.TypeLits(KnownNat,Nat(..))
import Data.Vector(Vector)
import Alpha.Base
import Alpha.Canonical.Algebra
import Alpha.Canonical.Functors
import Alpha.Canonical.Operators

class Sized (n::Nat) c e | e -> c where    

type Router a b = a -> b 

    
-- | Characterizes a type parameterized by some type 'a'  that is decomposable into a sequence a-values
class Decomposable a b where
    decompose::a -> [b]

class Infinite a where
    next::a -> a

class Structured a b where
    type Construction a b
    type Destructured a b    
    
    destructure::Construction a b -> Destructured a b    

-- | Characterizes a value that can be rendered in human-readable form
class Formattable a where
    format ::a -> Text

-- | Characterizes measurable things, in the spirit, but not formally, of Lebesque
class Measurable (n::Nat) a where
    measure::forall b. (Num b) => a -> b

class Length a where    
    length::forall b. (Num b) => a -> b
    
instance Length a => Measurable 1 a where
    measure = length
       
class Reversible a where
    reverse::a -> a


-- / Applicable when values can be joined via a binary function that need not be structure-preserving
class (Measurable n a) => Mixable n a where
    type Mixed n a 
    mix ::a -> a -> Mixed n a

-- / Applicable when values can be concatenated in a "structure-preserving" way
class Concatenable a b where
    type Concatenated a b
    concat :: a -> b -> Concatenated a b

    (+++) :: a -> b -> Concatenated a b
    (+++) x y = concat x y
    infixr 5 +++
    
-- | Characterizes a value that can be converted to a list of 'Text' values
class ToLines a where
    -- | Converts an 'a' value to a list of 'Text' values
    lines::a -> [Text]    
    
-- | Characterizes a value that can be converted to a 'String'
class ToString a where
    -- | Convers an 'a' value to a 'String'
    string::a -> String

-- | Characterizes a value that can be converted to a 'Bool'
class ToBool a where
    bool::a -> Bool    

-- | Characterizes a value that can be materialized from 'Text'
class FromText a where
    -- | Materializes an 'a'-value from text
    fromText::Text -> a
    
-- | Defines membership predicated on the ability to be counted by an existential machine
class Counted a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n

-- | Characterizes a finite container or other type that contains elements
-- that can be partitioned     
class Chunkable a where
    chunk::Int -> a -> [a]
    
-- / Breaking the chains..
class Jailbreak m a where
    escape::m a -> a

-- Removes a layer of enumerable structure    
-- In the case of a monoid, 'reduce' reduces to 'fold', pun intended
class Reducible a where
    reduce::[a] -> a
    
-- | Codifies a production relationship between an input value of type 'a' and 
-- an output value of type 'b'
class Producer a b where
    -- | Requires the constuction of a 'b' given an 'a' value
    make::a -> b

-- | Codifies a (directed) conversion relationship between an input value and output value
class Convertible a b where
    -- | Requires that an 'a' value be converted to a 'b' value
    convert::a -> b    
        
class Packable a b where
    pack::a -> b
    unpack::b -> a    
    
class Indexed c e where
    item::c -> Int -> e

    (!)::c -> Int -> e
    (!) = item
    
        
class Wrapped a b where
    unwrap::a -> b

-- A flow from a --> b
class Flow a b where
    flow::a -> Router a b -> b
    flow = (|>)

-- A covariant from a --> b
class Coflow a b where
    coflow::(a -> b) -> a -> b
    coflow = (<|)

class Enumerable c e | e -> c where 
    -- A source of type 'c' producing elements of type 'e'
    type Source c e
    
    items::Source c e -> [e]

class (Enumerable c e) =>  Singletary c e  where
    singleton::e -> Source c e
    
class (Eq e, Enumerable c e) => Existential c e  where
    exists::e -> Source c e -> Bool

    absent::e -> Source c e -> Bool
    absent item src = not (exists item src)

    
-- Characterizes a family of singleton types 'a' for which the type's single inhabitant
-- is reifiable
class Reifiable (a::k) r where
    reify::r 
        
class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  symstr @f |> T.pack
            
instance Jailbreak Maybe a where
    escape x = fromJust x
