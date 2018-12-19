-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Jailbreak
(
    Jailbreak(..),
)
where

import Alpha.Base
import Alpha.Canonical.Functions
import System.IO.Unsafe
import Control.Monad.Primitive

-- / Breaking the chains..
class Jailbreak m a where
    escape::m a -> a

instance Jailbreak Maybe a where
    escape x = fromJust x
    
instance Jailbreak IO a where
    escape = shredIO

instance (PrimBase m) => Jailbreak m a where
    escape x = x |> shredPrim
    