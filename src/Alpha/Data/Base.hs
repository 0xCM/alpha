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
    module X,
    Category, (.),
    Foldable, foldby, fold, foldr, foldr', foldl, foldl', null,
    Bool(..),(&&), (||), not, otherwise,
    Monoid(..),
    String,
    Text,
    HashSet,
    HashMap,
    Either(..),
    IsString,
    Ord,
    const,
    Coercible, coerce,
    Semigroup, (<>)
    

)
where
import Control.Category (Category, (.))
import Data.Foldable(Foldable, foldMap, fold, foldr, foldr', foldl, foldl', null)
import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Bits as X
import Data.Char as X
import Data.Data as X
import Data.Default as X
import Data.Coerce
import Data.Kind as X
import Data.Either.Combinators as X
import Data.Either(Either(..))
import Data.Eq as X
import Data.Functor as X
import Data.Int as X
import Data.Ix as X
import Data.Maybe as X
import Data.Ord(Ord)
import Data.String(String,IsString)
import Data.Semigroup
import Data.Text(Text)
import Data.Traversable as X
import Data.Typeable as X
import Data.Word as X
import Data.Monoid
import Data.HashSet(HashSet)
import Data.HashMap.Strict(HashMap)
import Data.Function(const)

-- | Folds a structure projected into a 'Monoid' by a supplied function
foldby :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldby = foldMap


