-----------------------------------------------------------------------------
-- | Stream manipulation
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Data.Stream
(
    Stream
)

where

import qualified Data.Stream.Infinite as IS
import qualified Data.Stream.Infinite as S
import Alpha.Canonical
import Alpha.Data.Base

type Stream = S.Stream

instance Enumerable (Stream e) e where
    type Source (Stream e) e = Stream e

    items s = S.takeWhile (\_ -> True) s


class (Enumerable (Stream e) e) => Streaming e where    
    tail::Stream e -> Stream e
    intersperse :: e -> Stream e -> Stream e
    iterate :: (e -> e) -> e -> Stream e
    
    
    
instance Streaming (Stream s)  where
    tail = S.tail
    intersperse = S.intersperse
    iterate = S.iterate
    
