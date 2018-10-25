-----------------------------------------------------------------------------
-- | Stream manipulation
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Data.Stream

where

import qualified Data.Stream.Infinite as IS
import Data.Stream.Infinite(Stream(..))
import Alpha.Canonical
import Alpha.Data.Base
import Alpha.Control.Base

instance Enumerable (Stream e) e where
    type Source (Stream e) e = Stream e

    items s = IS.takeWhile (\_ -> True) s


class Streaming s where    
    tail::s -> s
    
instance Streaming (Stream s)  where
    tail = IS.tail
