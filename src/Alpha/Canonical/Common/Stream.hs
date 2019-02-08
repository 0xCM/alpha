-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Common.Stream
(
    SequentialStream(..),
) where

import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual
import Alpha.Canonical.Common.Sequential
import Alpha.Canonical.Common.Container
import qualified Data.Stream.Infinite as Stream

class (Listed (Stream a), Weave a (Stream a), Iterable (Stream a) ) => SequentialStream a where    

    -- Constructs a sream that emits the elements
    -- of a list, cyling over said elements indefinitely
    -- if the list is finite
    cycle::[a] -> Stream a

    -- Constructs a stream via opaque function calls
    blackbox::(() -> a) -> Stream a

instance SequentialStream (Stream s)  where
    cycle (x:xs) = Stream.cycle(x :| xs)
    blackbox f = Stream.iterate (\_ -> f ()) (f())        
        
instance IsList (Stream a) where
    type Item (Stream a) = a
    toList = Stream.takeWhile (\_ -> True)
    fromList [x] = Stream.cycle [x]

