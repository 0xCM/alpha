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
import Data.List.NonEmpty

type Stream = S.Stream

instance Enumerable (Stream e) e where
    type Source (Stream e) e = Stream e

    items s = S.takeWhile (\_ -> True) s


class (Enumerable (Stream e) e) => Streaming e where    
    -- Skips the leading element and returns the remainder
    tail::Stream e -> Stream e
    -- Constructs a new stream by interspersing a specific element with an existin stream
    intersperse :: e -> Stream e -> Stream e    
    iterate :: (e -> e) -> e -> Stream e
    cycle::[e] -> Stream e
    
    
    
instance Streaming (Stream s)  where
    tail = S.tail
    intersperse = S.intersperse
    iterate = S.iterate
    cycle (x:xs) = S.cycle(x :| xs)
    
