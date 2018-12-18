{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Relations.Subset
(    
    Subset(..)

) where

import Alpha.Base

class Universe u where

class Subset a b where

-- Reflexivity
instance Subset a a

-- Transitivity
instance (Subset a b, Subset b c) => Subset a c

instance Subset Int8 Int16
instance Subset Int16 Int32
instance Subset Int32 Int64
instance Subset Int64 Integer
instance Subset Word8 Word16
instance Subset Word16 Word32
instance Subset Word32 Word64
instance Subset Word64 Natural
instance Subset Natural Integer


