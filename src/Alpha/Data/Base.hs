-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE MagicHash #-}

module Alpha.Data.Base
(
    Arg, ArgMin, ArgMax,
    FiniteBits(..),
    Bool(..),(&&), (||), not, otherwise,
    Monoid(..),
    Dual(..), Endo(..), All(..), Any(..), Sum(..),
    Groupoid(..),
    String, Text,
    HashSet,HashMap,
    Map, associate,
    Either(..),
    IsString,
    Ord, Ordering,
    const,
    Coercible(..), coerce,
    Semigroup(..), Min(..), Max(..), First(..), Last(..),
    Data(..),
    Type,
    Char,chr,intToDigit,
    Functor(..),
    Default(..),
    Eq(..),
    Seq,
    Traversable(..),
    Typeable(..), Proxy(..),
    Maybe(..), isNothing, fromJust,
    Ix(..),
    Int,Int8,Int16,Int32,Int64,
    Word,Word8,Word16,Word32,Word64,
    Integer,
    Rational(..),
    Fractional(..),
    Floating(..), RealFloat(..),
    Double , Double#, Float, Float#
    
)
where
import Control.Category (Category((.), id))
import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Bits(FiniteBits(..))
import Data.Char(Char,chr,intToDigit)
import Data.Coerce(Coercible(..),coerce)
import Data.Data(Data(..))
import Data.Monoid(Monoid(mempty, mappend,mconcat))
import Data.Monoid(Dual(..), Endo(..), All(..), Any(..),  Sum(..),Alt(..))
import Data.Default(Default(def))
import Data.Either(Either(..))
import Data.Eq(Eq(..),(==),(/=))
import Data.Foldable(Foldable, foldMap, fold, foldr, foldr', foldl, foldl')
import Data.Functor(Functor(..))
import Data.Groupoid(Groupoid(..))
import Data.HashSet(HashSet)
import Data.HashMap.Strict(HashMap)
import Data.Int(Int,Int8,Int16,Int32,Int64)
import Data.Ix(Ix(..))
import Data.Kind(Type)
import Data.Map.Strict(Map)
import Data.Maybe(Maybe(..),isNothing,fromJust)
import Data.Monoid(Monoid(..))
import Data.Ord(Ord, Ordering)
import Data.String(String,IsString)
import Data.Sequence(Seq)
import Data.Semigroup(Semigroup(..), Min(..), Max(..), First(..), Last(..),(<>), sconcat)
import Data.Text(Text)
import Data.Traversable(Traversable(..))
import Data.Typeable(Typeable(..),Proxy(..))
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Data.Function(const)
import Data.Semigroup(Arg,ArgMin,ArgMax)
import Data.Ratio
import GHC.Float
import GHC.Num
import GHC.Real(Fractional(..))
import qualified Data.Map.Strict as Map

-- | Folds a structure projected into a 'Monoid' by a supplied function
foldby :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldby = foldMap

-- | Produces an associative array for a list of key-value pairs
associate::(Ord k) => [(k,v)] -> Map k v
associate = Map.fromList
