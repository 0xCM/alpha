-----------------------------------------------------------------------------
-- | Common system-level abstractions and utilities
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.System
(shredIO)
where

import System.IO
import System.IO.Unsafe

-- | Just say "no" to the monolithic imprisonment of IO
shredIO :: IO a -> a
shredIO = unsafeDupablePerformIO 
