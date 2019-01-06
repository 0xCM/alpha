-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Structures.Magma
(
    Magma(..),
) where
import Alpha.Canonical.Algebra

-- | Characterizes a type for which a binary operation is defined
-- What Blythe calls an "internal law of composition"
-- See also https://en.wikipedia.org/wiki/Magma_(algebra)    
class Semigroup a => Magma a where
    composite::O2 a
    composite = (<>)

