-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Data.Base
(
    FiniteBits(..),
    Category, (.),
    Foldable, foldby, fold, foldr, foldr', foldl, foldl',
    Bool(..),(&&), (||), not, otherwise,
    Monoid(..),
    String,
    Text,
    HashSet,
    HashMap,
    Either(..),
    IsString,
    Ord, Ordering,
    const,
    Coercible, coerce,
    Semigroup, (<>), sconcat,
    Data(..),
    Type,
    Int,Int8,Int16,Int32,Int64,
    Word,Word8,Word16,Word32,Word64,
    Char,chr,intToDigit,
    Functor(..),
    Default, def,
    Eq(..),
    Traversable(..),
    Typeable(..), Proxy(..),
    Maybe(..), isNothing, fromJust,
    Ix(..)
    

)
where
import Control.Category (Category, (.), id)
import Data.Foldable(Foldable, foldMap, fold, foldr, foldr', foldl, foldl')
import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Bits(FiniteBits(..))
import Data.Char(Char,chr,intToDigit)
import Data.Data(Data(..))
import Data.Default(Default,def)
import Data.Coerce(Coercible,coerce)
import Data.Kind(Type)
import Data.Either(Either(..))
import Data.Eq(Eq(..),(==),(/=))
import Data.Functor(Functor(..))
import Data.Int(Int,Int8,Int16,Int32,Int64)
import Data.Ix(Ix(..))
import Data.Maybe(Maybe(..),isNothing,fromJust)
import Data.Ord(Ord, Ordering)
import Data.String(String,IsString)
import Data.Semigroup(Semigroup, (<>), sconcat)
import Data.Text(Text)
import Data.Traversable(Traversable(..))
import Data.Typeable(Typeable(..),Proxy(..))
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Data.Monoid(Monoid(..))
import Data.HashSet(HashSet)
import Data.HashMap.Strict(HashMap)
import Data.Function(const)

-- | Folds a structure projected into a 'Monoid' by a supplied function
foldby :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldby = foldMap


