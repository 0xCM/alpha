{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Types
(
    Coercible(..), 
    Coercion(..),
    Data(..),
    Typeable(..), Proxy(..),
    Generic(..),
    Generic1(..),    
    Type,
    Default(..),
    proxy,
)
where

import Data.Coerce(Coercible(..))
import Data.Data(Data(..))
import Data.Kind(Type)
import Data.Proxy
import Data.Typeable(Typeable(..),Proxy(..))
import Data.Type.Coercion
import Data.Default(Default(def))
import GHC.Generics(Generic(..),Generic1(..))
    
proxy:: Proxy n
proxy = Proxy
{-# INLINE proxy #-}
