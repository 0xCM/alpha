module Alpha.Jailbreak
(
    Jailbreak(..),
)
where

import Alpha.Base
import Alpha.Canonical.Operators
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
    