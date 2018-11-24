-----------------------------------------------------------------------------
-- | Defines vectcor-related API surface
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Data.Vector
(
    Vector(..), 
    Vectored(..), 
    Covectored(..),
    unsized, 
    vector    
)
where
import qualified Data.Vector as V
import Alpha.Base
import Alpha.Data.Product
import Alpha.Canonical
import Alpha.Data.Numbers
import qualified Data.List as List

-- | Represents a sized vector
data Vector (n::Nat) a 
    = Col (V.Vector a)
    | Row (V.Vector a)
        deriving (Show,Generic,Data,Typeable)    

-- | Characterizes types that can be expressed as a sized vector        
class Vectored a b where        
    -- Creates a column vector from an a-value
    col::a -> Vector n b

-- | Characterizes types that can be expressed as a sized covector        
class Covectored a b where
    -- Creates a row vector from an a-value
    row::a -> Vector n b

-- | Removes the size adornment from a sized vector
unsized::Vector n a -> V.Vector a
unsized (Col v) = v
unsized (Row v) = v

-- | Converts a vector to a covector and conversely
transpose::Vector n a -> Vector n a
transpose  (Col v) = Row v
transpose  (Row v) = Col v

-- Creates vector from a list    
vector::[a] -> Vector n a
vector src = Col(V.fromList src)

-- Creates a covector from a list    
covector::[a] -> Vector n a
covector src = Row(V.fromList src)

instance Length (Vector n a) where
    length = convert . V.length . unsized

instance Indexed (Vector n a) Int a where
    item (Col a) i = a V.! i
    item (Row a) i = a V.! i

instance Vectored (a,a) a where
    col (x1, x2) = Col $ V.fromList [x1, x2]
instance Vectored (a, a, a) a where
    col (x1, x2, x3) = Col $ V.fromList [x1, x2, x3]
instance Vectored (a, a, a, a) a where
    col (x1, x2, x3, x4) = Col $ V.fromList [x1, x2, x3, x4]
instance Vectored (a, a, a, a, a) a where
    col (x1, x2, x3, x4, x5) = Col $ V.fromList [x1, x2, x3, x4, x5]
instance Vectored (a, a, a, a, a, a) a where
    col (x1, x2, x3, x4, x5, x6) = Col $ V.fromList [x1, x2, x3, x4, x5, x6]
instance Vectored (a, a, a, a, a, a, a) a where
    col (x1, x2, x3, x4, x5, x6, x7) = Col $ V.fromList [x1, x2, x3, x4, x5, x6, x7]
                
instance Covectored (a,a) a where
    row (x1, x2) = Row $ V.fromList [x1, x2]    
instance Covectored (a,a,a) a where    
    row (x1, x2, x3) = Row $ V.fromList [x1, x2, x3]    
instance Covectored (a, a, a, a) a where    
    row (x1, x2, x3, x4) = Row $ V.fromList [x1, x2, x3, x4]     
instance Covectored (a, a, a, a, a) a where    
    row (x1, x2, x3, x4, x5) = Row $ V.fromList [x1, x2, x3, x4, x5]            
instance Covectored (a, a, a, a, a, a) a where    
    row (x1, x2, x3, x4, x5, x6) = Row $ V.fromList [x1, x2, x3, x4, x5, x6]        
instance Covectored (a, a, a, a, a, a, a) a where    
    row (x1, x2, x3, x4, x5, x6, x7) = Row $ V.fromList [x1, x2, x3, x4, x5, x6, x7]        
        