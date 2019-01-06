-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Collective.Stream
(
    SequentialStream(..),
) where

import Alpha.Canonical.Relations
import Alpha.Canonical.Collective.Container
import qualified Data.Stream.Infinite as Stream

class (Headed (Stream a), Weave a (Stream a), Iterable (Stream a) ) => SequentialStream a where    

    -- Constructs a sream that emits the elements
    -- of a list, cyling over said elements indefinitely
    -- if the list is finite
    cycle::[a] -> Stream a

    -- Constructs a stream via opaque function calls
    blackbox::(() -> a) -> Stream a


type instance Individual (Stream a) = a

    
instance IsList (Stream a) where
    type Item (Stream a) = a
    toList = Stream.takeWhile (\_ -> True)
    fromList [x] = Stream.cycle [x]

instance Container (Stream a) where
    contain [x] = Stream.cycle [x]
    contents s = Stream.takeWhile (\_ -> True) s
    
instance SequentialStream (Stream s)  where
    cycle (x:xs) = Stream.cycle(x :| xs)
    blackbox f = Stream.iterate (\_ -> f ()) (f())        

instance Singletary (Stream a) where
    singleton x = Stream.cycle [x]
        
instance Headed (Stream a) where        
    head s = s Stream.!! 0
    tail = Stream.tail

instance Iterable (Stream a) where
    iterate = Stream.iterate    
            
instance Weave g (Stream g) where
    weave = Stream.intersperse
    