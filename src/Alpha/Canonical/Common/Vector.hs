-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Common.Vector
(
    module X,
    VecPair(..),
    vmix,
    vecpair,
)
where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual as X
import Alpha.Canonical.Common.Conversions as X
import Alpha.Canonical.Common.Format as X
import Alpha.Canonical.Common.Partnership as X

import qualified Data.Vector as Vector
import qualified Data.List as List

-- | Represents a pair of vectors
newtype VecPair a = VecPair (Vector a, Vector a)
    deriving (Eq,Ord,Generic,Data,Typeable)
instance Newtype (VecPair a)    

type instance Individual (Vector a) = a
type instance Individual (VecPair a) = (a,a)

type instance Partner 0 (VecPair a) = Vector a
type instance Partner 1 (VecPair a) = Vector a

    
-- | Reduces a pair of vectors to a single vector via a binary operator
vmix::(a -> a -> a) -> VecPair a -> Vector a    
vmix f v = Vector.fromList ((\(x,y) -> f x y) <$> individuals v)


vecpair::Vector a -> Vector a -> VecPair a
vecpair v1 v2 = VecPair (v1,v2)

instance Discrete (VecPair a) where
    individuals (VecPair (v1,v2)) = Vector.zipWith (\x y -> (x,y)) v1 v2 |> toList

instance Discrete (Vector a) where
    individuals = toList
        
instance Vectored (Vector a) where
    vector  = id    

instance (Formattable a) => Formattable (Vector a) where
    format v = v |> individuals |> tuplestring
    
instance Partnership 0 (VecPair a) where
    partner (VecPair (a,_)) = a

instance Partnership 1 (VecPair a) where
    partner (VecPair (_,b)) = b
    