-----------------------------------------------------------------------------
-- | Defines a 2-dimensional tabular data structure
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Table
(
    Tabular(..)
)
where
import qualified Data.Matrix as M
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Product
import Alpha.Data.Numbers
import Alpha.Data.Natural

import qualified Data.List as List

-- | Characterizes a rectangular array of data
class Tabular a where
    -- The data target
    type Table a
    -- The data source
    type TableSource a
    -- Constructs a table from a source
    table::TableSource a -> Table a
        
instance Tabular (MatrixTable m n a) where
    type Table (MatrixTable m n a) = MatrixTable m n a
    type TableSource (MatrixTable m n a) = [[a]]
    table = table'
    
-- | Specifies the form of the matrix dimension type
type TableDim = (Int, Int)

-- | Defines a matrix with type-level indexes
newtype MatrixTable m n a = MatrixTable (M.Matrix a)    
    deriving (Eq, NFData, Functor, Semigroup, Monoid, Applicative, Foldable, Traversable, Num) 

table'::forall m n a. [[a]] -> MatrixTable m n a
table' elements = MatrixTable $ M.fromLists elements
    
--instance Container (MatrixTable m n a)    
instance (Show a) => Show(MatrixTable m n a) where
    show (MatrixTable m) = show m

instance forall m n a.NatPair m n  =>  IsList (MatrixTable m n a) where
    type Item (MatrixTable m n a) = a
    toList (MatrixTable m) = M.toList m
    fromList elements = p |> table'
        where 
            width = int $ nat @n
            p = partition width elements 

instance forall m n a. (KnownNat m, KnownNat n) => Container (MatrixTable m n a) where
    contain = fromList
    contents = toList

instance forall m n a. (KnownNat m, KnownNat n, Nullary a, Num a) => Nullary (MatrixTable m n a) where
    zero = MatrixTable (M.zero rv cv) where
        rv = int $ natVal $ Proxy @m 
        cv = int $ natVal $ Proxy @n

instance forall m a. (KnownNat m, Nullary a, Num a) => Unital (MatrixTable m m a) where
    one = MatrixTable (M.identity rv ) where
        rv = int $ natVal $ Proxy @m

instance forall m n a. (KnownNat m, KnownNat n) => Dimensional (MatrixTable m n a) where
    type Dimension (MatrixTable m n a) = TableDim

    dimension (MatrixTable m) = (rv, cv) where
        rv = int $ natVal $ Proxy @m 
        cv = int $ natVal $ Proxy @n

instance Show a => Formattable (MatrixTable m n a) where
    format = pack . show

instance Indexed (MatrixTable m n a) (Int,Int) where
    type Found (MatrixTable m n a) (Int,Int) = a
    lookup (MatrixTable m) (r,c) = M.getElem r c m