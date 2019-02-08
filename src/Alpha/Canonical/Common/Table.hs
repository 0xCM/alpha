-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Table
(
    Tabled(..),
    DataTable(..),
) where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual
import qualified Data.Matrix as M

-- | Characterizes a rectangular array of data
class Tabled a where
    -- The data target
    type Table a
    -- The data source
    type TableSource a
    -- Constructs a table from a source
    table::TableSource a -> Table a

-- | Defines a matrix with type-level indexes
newtype DataTable m n a = DataTable (M.Matrix a)    
    deriving (Eq, NFData, Functor, Semigroup, Monoid, Applicative, Foldable, Traversable, Num) 
    
type instance Individual (DataTable m n a) = a

-- | Specifies the form of the matrix dimension type
type TableDim = (Int, Int)

instance forall m n a. (KnownNat m, KnownNat n) => Dimensional (DataTable m n a) where
    type instance Dimension (DataTable m n a) = TableDim
    dimension _ = (nat @m, nat @n)

table'::forall m n a. [[a]] -> DataTable m n a
table' elements = DataTable $ M.fromLists elements
    
instance Tabled (DataTable m n a) where
    type Table (DataTable m n a) = DataTable m n a
    type TableSource (DataTable m n a) = [[a]]
    table = table'

instance (Show a) => Show(DataTable m n a) where
    show (DataTable m) = show m


instance forall m n a.(KnownNat m, KnownNat n) =>  IsList (DataTable m n a) where
    type Item (DataTable m n a) = a
    toList (DataTable m) = M.toList m
    fromList elements = p |> table'
        where 
            width = fromIntegral $ nat @n
            p = segment width elements 

