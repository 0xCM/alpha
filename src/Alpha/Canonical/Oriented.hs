-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Oriented
(
    Reversible(..),
    Transposable(..)

) where
import Alpha.Base
import qualified Data.List as List  

-- | Characterizes a type that manifests the concept
-- of an invertible reversion    
class Reversible a b | a -> b, b -> a  where
    reverse::a -> b

-- Characterizes structures that support a notion of duality such that
-- transpose . transpose = id
-- Canonical examples are vectors and matrices
class Transposable a where
    type Transposed a
    type Transposed a = a

    transpose::a -> Transposed a


instance Reversible [a] [a] where
    reverse = List.reverse
    
instance Transposable [a] where
    transpose = reverse
    