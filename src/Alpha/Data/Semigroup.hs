-----------------------------------------------------------------------------
-- | Semigroup extensions
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Alpha.Data.Semigroup where

import Alpha.Canonical

instance Wrapped (Dual a) a where
    unwrap = getDual
    
instance Wrapped (Endo a) (a->a) where
    unwrap = appEndo

instance Wrapped (Sum a) a where
    unwrap = getSum
    
