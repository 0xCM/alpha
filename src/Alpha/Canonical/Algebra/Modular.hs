-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Algebra.Modular
(
    Modulo(..),
    Bimodular(..),(/%), (/%%),

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Common

import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Set as Set

-- Represents a family of type pairs that support a notion of the first 
-- type 'mod' the second type. Intended to represent to the result of the 
-- modulus operation on integers, a paritioning of a set by a subset or
-- more generally, quotient groups and similar
type family Modulo a b

class Bimodular a b where
    -- | Calculates the remainder of dividing the first operand by the second
    bimod::a -> b -> Modulo a b

    -- | Infix synonym for 'bimod'
    (>%<)::a -> b -> Modulo a b
    (>%<) = bimod
    {-# INLINE (>%<) #-}
    infix 8 >%<
    
(/%)::(Integral n) => n -> n -> (n,n)
(/%) = quotRem'
{-# INLINE (/%) #-}
infixl 5 /%

(/%%)::(Integral n) => n -> n -> (n,n)
(/%%) = divMod'
{-# INLINE (/%%) #-}
infixl 5 /%%
            