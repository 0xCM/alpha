-----------------------------------------------------------------------------
-- | Defines facilities to represent and manipulate monomorphic tuples
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------

module Alpha.Data.MTuple 
(
    MTuple
)
where

import Alpha.Base
import Alpha.Canonical

type family MTuple a        
type instance MTuple (x, x) = (x, x)
type instance MTuple (x, x, x) = (x, x, x)
type instance MTuple (x, x, x, x) = (x, x, x, x)
type instance MTuple (x, x, x, x, x)  = (x ,x, x, x, x)
type instance MTuple (x, x, x, x, x, x)  = (x ,x, x, x, x, x)
type instance MTuple (x, x, x, x, x, x, x)  = (x ,x, x, x, x, x, x)
type instance MTuple (x, x, x, x, x, x, x, x)  = (x ,x, x, x, x, x, x, x)
type instance MTuple (x, x, x, x, x, x, x, x, x)  = (x ,x, x, x, x, x, x, x, x)


