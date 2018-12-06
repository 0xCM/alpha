-----------------------------------------------------------------------------
-- | Stream manipulation
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Stream
(
    Stream, Streamer
)
where

import qualified Data.Stream.Infinite as IS
import qualified Data.Stream.Infinite as S
import Data.Stream.Infinite
import Data.List.NonEmpty( NonEmpty((:|)) )

import Alpha.Canonical
import Alpha.Base
import Alpha.Data.Numbers
import Alpha.Data.Seq


type SeqStream e = Sequential (Stream e) 

class SeqStream a => Streamer a where    

    -- Constructs a new stream by interspersing a specific element with an existing stream
    intersperse::a -> Stream a -> Stream a    
    
    -- Constructs a stream by successive function applications
    -- to an initial value, i.e. ...f (f (a))
    iterate::(a -> a) -> a -> Stream a
    
    -- Constructs a sream that emits the elements
    -- of a list, cyling over said elements indefinitely
    -- if the list is finite
    cycle::[a] -> Stream a

    -- Constructs a stream via opaque function calls
    blackbox::(() -> a) -> Stream a

instance IsList (Stream a) where
    type Item (Stream a) = a
    toList = S.takeWhile (\_ -> True)
    fromList [x] = S.cycle [x]

instance Container (Stream e) where
    singleton x = S.cycle [x]
    contain [x] = S.cycle [x]
    contents s = S.takeWhile (\_ -> True) s
        
instance Sequential (Stream e) where    
    take i s = fromList $ S.take (fromIntegral i) s
    split = S.partition
    while p s = undefined
    tail = S.tail
    skip n s = S.drop (fromIntegral n) s

instance Streamer (Stream s)  where
    intersperse = S.intersperse
    iterate = S.iterate
    cycle (x:xs) = S.cycle(x :| xs)
    blackbox f = S.iterate (\_ -> f ()) (f())        

instance (Eq a) => Filterable (Stream a) where
    filter = S.filter
    