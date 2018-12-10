-----------------------------------------------------------------------------
-- | Defines a 2-dimensional tabular data structure
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Tabular
(
    Tabular(..),
    DataTable(..)
)
where
import qualified Data.Matrix as M
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Conversion
import Alpha.Data.NatK
import qualified Data.Text as Text
import qualified Data.List as List

-- | Characterizes a rectangular array of data
class Tabular a where
    -- The data target
    type Table a
    -- The data source
    type TableSource a
    -- Constructs a table from a source
    table::TableSource a -> Table a

-- | Defines a matrix with type-level indexes
newtype DataTable m n a = DataTable (M.Matrix a)    
    deriving (Eq, NFData, Functor, Semigroup, Monoid, Applicative, Foldable, Traversable, Num) 
    
instance Tabular (DataTable m n a) where
    type Table (DataTable m n a) = DataTable m n a
    type TableSource (DataTable m n a) = [[a]]
    table = table'

type instance Element (DataTable m n a) = a
    
-- | Specifies the form of the matrix dimension type
type TableDim = (Int, Int)


table'::forall m n a. [[a]] -> DataTable m n a
table' elements = DataTable $ M.fromLists elements
    
--instance Container (DataTable m n a)    
instance (Show a) => Show(DataTable m n a) where
    show (DataTable m) = show m


instance forall m n a.KnownNatPair m n  =>  IsList (DataTable m n a) where
    type Item (DataTable m n a) = a
    toList (DataTable m) = M.toList m
    fromList elements = p |> table'
        where 
            width = int $ nat @n
            p = partition width elements 

instance forall m n a. (KnownNat m, KnownNat n) => Container (DataTable m n a) where
    contain = fromList
    contents = toList

instance forall m n a. (KnownNat m, KnownNat n, Nullary a, Num a) => Nullary (DataTable m n a) where
    zero = DataTable (M.zero rv cv) where
        rv = natg @m
        cv = natg @n

instance forall m a. (KnownNat m, Nullary a, Num a) => Unital (DataTable m m a) where
    one = DataTable (M.identity rv ) where
        rv = natg @m

instance forall m n a. (KnownNat m, KnownNat n) => Dimensional (DataTable m n a) where
    type Dimension (DataTable m n a) = TableDim

    dimension (DataTable m) = (natg @m, natg @n) where
        
instance Show a => Formattable (DataTable m n a) where
    format = Text.pack . show

instance forall m n a.KnownNatPair m n  => Indexed (DataTable m n a) (Int,Int) where
    lookup (DataTable m) (r,c) = M.getElem r c m