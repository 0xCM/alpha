-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Common.Cardinality
(
    module X,
    Cardinality(..),   
    Cardinal(..),
    Infinity(..),
    Indeterminate(..),
    neginf,
    posinf,

)
where
import Alpha.Canonical.Common.Orientation as X

-- | Represents either negative or positive infinity
newtype Infinity a = Infinity Orientation
    deriving (Eq, Ord, Generic, Data, Typeable)
instance Newtype (Infinity a)    

-- | Classifies a type to which a cardinality may be assigned    
class Cardinal a where

    -- | Determines the cardinality of 'a'
    cardinality::a -> Cardinality

    isFinite::a -> Bool
    isFinite a = (cardinality a) != Infinite

    isUnbounded::a -> Bool
    isUnbounded a = (cardinality a) == Infinite

    isEmpty::a -> Bool
    isEmpty a = (cardinality a) == Zero

-- | Specifies the cardinality of a set and partitions the universe
-- of sets under consideration
-- See https://en.wikipedia.org/wiki/Cardinality
data Cardinality =
      Zero
    -- |^ There are no elements
    | Finite
   -- |^ A finite, nonzero number of elements
    | Infinite
   -- |^ A countably-infinite number of elements
   deriving (Eq, Ord, Generic, Data, Typeable, Enum)

-- | Represents an indeterminate form
-- See https://en.wikipedia.org/wiki/Indeterminate_form
data Indeterminate a =
    ZedDivZed
    -- |^ Represents the quotient 0/0
  | InfDivInf (Infinity a,Infinity a)
   -- |^ Represents the quotient 0/inf
  | ZedMulInf (Infinity a)
  -- |^ Represents the product 0*inf
  | InfSubInf (Infinity a, Infinity a)
  -- |^ Represents the difference inf-inf
  | ZedPowZed
  -- |^ Represents 0 raised to the 0th power
  | InfPowZed (Infinity a)
  -- |^ Represents inf raised to the 0th power
  deriving (Eq, Ord, Generic, Data, Typeable)

-- | Constructs a negative infinity value
neginf::Infinity a
neginf = Infinity Leftward
{-# INLINE neginf #-}

-- | Constructs a positive infinity value
posinf::Infinity a
posinf = Infinity Rightward
{-# INLINE posinf #-}


instance Formattable a => Formattable (Indeterminate a)  where
    format ZedDivZed = D0 <> FSlash <> D0
    format (InfDivInf (num,denom)) = format num <> FSlash <> format denom
    format (ZedMulInf inf) = D0 <> Dash <> format inf
    format (InfSubInf (linf, rinf)) = format linf <> Dash <> format rinf
    format ZedPowZed = D0 <> Caret <> D0
    format (InfPowZed inf) = (format inf) <> Caret <> D0

instance Formattable a => Show (Indeterminate a)  where
    show = string . format    

instance Formattable a => Formattable (Infinity a)  where
    format (Infinity (Leftward))  = Dash <> "inf"
    format (Infinity (Rightward)) = Plus <> "inf"
        
instance Formattable a => Show (Infinity a)  where
    show = string . format