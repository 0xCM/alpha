-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}

module Alpha.Canonical.Structures.Common
(
    module X,
    EndSet(..),
    HomSet(..),
    ObjSet(..),
    Variable(..), 
    VarVal(..),
    var, 
    varval, 

) where
import Alpha.Canonical.Algebra as X


-- Represents families of endomorphisms within the same category 
data family EndSet (c::Constraint) (x::Constraint) (y::Constraint)

-- Represents families of morphisms from a category c to a category d
data family HomSet (c::Constraint) (x::Constraint) (d::Constraint) (y::Constraint)

-- | Represents families of (categorical) objects
data family ObjSet (c::Constraint)


-- | Represents an a-valued variable labeled via a type-level symbol
data Variable s a = Variable
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 

-- | Specifies a value for a variable
newtype VarVal s a = VarVal a
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 

-- | Constructs a variable    
var::KnownSymbol s => Variable s a
var = Variable

-- | Constructs a variable value   
varval::KnownSymbol s => a -> VarVal s a
varval = VarVal

-- type instance Individual (Interval a) = a

-- instance Subtractive a => Length (Interval a) where
--     length i = supremum i - infimum i
