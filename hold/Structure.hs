-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Elementary.Structure
(
    
)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Set

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Sequence as Sequence



class Structure (a::Constraint) (i::Type) where
    
