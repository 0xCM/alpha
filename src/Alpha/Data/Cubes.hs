-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Cubes
(
    Cube(..),
    cube,
    ElementaryInterval(..),
    ElementaryCube(..),
    embeddingNumber,
    elpoint,
    elinterval
)
where
import Alpha.Canonical

-- See p. 40 of Y2004CHOMO

-- | Represents an n-dimensional cube
newtype Cube n a = Cube (VecN n (Interval a))
    deriving (Eq,Ord,Generic,Data,Typeable,Discrete)

type instance Individual (Cube n a) = Interval a

-- | Constructs an n-dimensional cube
cube::forall n a. KnownNat n => [Interval a] -> Cube n a
cube intervals = Cube $ vecN intervals

-- | Represents a closed interval of the form [a, a + one] or [a,a] which is degenerate
-- and represents a singleton
newtype ElementaryInterval a = ElementaryInterval (Interval a)
    deriving (Eq,Ord,Generic,Data,Typeable)

-- | Represents an n-dimensional cube composed of n 'ElementaryInterval' values
newtype ElementaryCube n a = ElementaryCube (VecN n (ElementaryInterval a))
    deriving (Eq,Ord,Generic,Data,Typeable,Discrete)

type instance Individual (ElementaryInterval a) = a
type instance Individual (ElementaryCube n a) = ElementaryInterval a        

-- | The embedding number of the cube is defined to be the number of components
-- and this correlates with the dimenion n of Rn in which the cube is situated
embeddingNumber::forall n a. KnownNat n => ElementaryCube n a -> Int
embeddingNumber _ = natg @n

-- | Constructs a nondegenerate 'ElementaryInterval'
elinterval::OrdSemiring a => a -> ElementaryInterval a
elinterval min = seminterval min (min + one) |> ElementaryInterval

-- | Constructs a degenerate 'ElementaryInterval'
elpoint::(OrdSemiring a) => a -> ElementaryInterval a
elpoint p = seminterval p p |> ElementaryInterval

-- | Constructs a 'ElementaryCube'
elcube::forall n a. (KnownNat n, OrdSemiring a) => VecN n (ElementaryInterval a) -> ElementaryCube n a
elcube edges = ElementaryCube edges

instance Indexable (ElementaryCube n a) where
    idx (ElementaryCube vector) i = vector !! i
    
-- | The dimenion of an elementary cube is defined to be the count of nondegenerate components    
instance forall n a. (KnownNat n, Ord a) => Dimensional (ElementaryCube n a) where
    type Dimension (ElementaryCube n a) = Natural
    dimension (ElementaryCube components) 
        = components |> filter (\c -> not (degenerate c)) |> length
        
instance (Ord a) => Degenerate (ElementaryInterval a) where
    degenerate (ElementaryInterval x) = infimum x == supremum x
