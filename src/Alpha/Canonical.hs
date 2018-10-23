-----------------------------------------------------------------------------
-- | Fundamental constructs
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Alpha.Canonical 
(
    Concatenable(..),
    Decomposable(..),
    ToString(..),
    Formattable(..),
    FromText(..),
    Chain(..),
    Counted(..),
    Jailbreak(..),
    Producer(..),
    Convertible(..),
    Infimum(..),
    Supremum(..),
    Lazy(..),
    Eager(..),
    Indexed(..),
    Packable(..),
    Length(..),
    Collapsable(..),
    Proxy(..),
    ToLines(..),
    ToBool(..),
    module Alpha.Operators,
    module Alpha.Algebra,
    module Alpha.Functors,
    shredIO,IO,
    proxy,
    ifelse,
    NatPair(..),
    Nat
)
where


import System.IO(IO)
import System.IO.Unsafe

import Data.Text(Text)
import Data.String(String)
import Data.Int(Int)
import GHC.TypeLits
import Alpha.Operators
import Data.Proxy
import Data.Maybe
import Data.Ord
import Data.Word
import Data.Bool
import Data.Kind(Type)
import Data.Vector(Vector)
import Alpha.Algebra
import Alpha.Functors
import GHC.Num(Num)
import GHC.Real(Integral)


-- | Just say "no" to the monolithic imprisonment of IO
shredIO :: IO a -> a
shredIO = unsafePerformIO

-- | If the first input value is true, returns the 2nd input value,
-- otherwise, returns the third input value
ifelse::Bool -> a -> a -> a
ifelse x aye no = case x of
            True -> aye
            _ -> no

-- / Constrains a pair of types to be known naturals 
type NatPair n1 n2 = (KnownNat n1, KnownNat n2)

-- | Characterizes a type parameterized by some type 'a'  that is decomposable into a sequence a-values
class Decomposable a b where
    decompose::a -> [b]

class Infinite a where
    next::a -> a

-- | Characterizes a value that can be rendered in human-readable form
class Formattable a where
    format ::a -> Text

-- | Characterizes measurable things, in the spirit, but not formally, of Lebesque
class Measurable (n::Nat) a where
    measure::forall b. (Integral b) => a -> b

class Length a where    
    length::forall b. (Integral b) => a -> b
    
instance Length a => Measurable 1 a where
    measure = length

       
class Collapsable a where
    collapse::[a] -> a

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

class Chain a b where
    lchain::a->b
    rchain::b->a
    
-- | Defines membership predicated on the ability to be counted by an existential machine
class Counted a where
    -- | Counts the number of items within the purview of the subject
    count::a -> Int
    
    
-- / Breaking the chains..
class Jailbreak m a where
    escape::m a -> a

instance Jailbreak Maybe a where
    escape x = fromJust x

class (Ord a) => Infimum a where
    -- / The greatest lower bound
    infimum::a    
    
class (Ord a) => Supremum a where
    -- / The least upper bound
    supremum::a
    
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
    
class Paired a b where
    type Pair a b
    type Pair a b = (a, b)

    pair::a -> b -> Pair a b
        
    

class Wrapped a b where
    unwrap::a -> b

-- Characterizes a sequence of bytes
-- class (Length a, Packable [Word8] a, Concatenable a a) => Bytes a where

--     item:: a -> Int -> Word8

-- instance (Bytes a) => Indexed a Word8 where
--     --(!) :: a -> Int -> Word8
--     (!) = item



-- Characterizes a key-value store
class Store k v where
    put::k -> v -> ()
    get::k -> v    
    del::k -> ()

type Router a b = a -> b    

class Target a b where
    push::a -> Router a b -> b
    push = (|>)

class Source a b where
    pull::Router a b -> a -> b
    pull = (<|)
    
-- | Manufactures a proxy for an 'a'-value
proxy::forall a. Proxy a
proxy = Proxy @a

map::(Functor f) => (a -> b) -> f a -> f b
map = fmap

empty::(Monoid m) => m
empty = mempty


type family Lazy a 

type family Eager a

type family Boxed a

type family Raw a