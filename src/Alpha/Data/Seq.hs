-----------------------------------------------------------------------------
-- | A pattern for sequential containers
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Seq
(
    Seq(..),
    Sequential(..)
)
where

import Alpha.Base
import Alpha.Canonical
import Data.Sequence as S
import Prelude(fst,snd)
    

instance Container (Seq a) where
    contain x = S.fromList x
    contents = toList

instance Sequential (Seq a) where    
    take n s =  S.take (fromIntegral n) s
    tail s = snd $ S.splitAt 1 s
    while = S.takeWhileL 
    split = S.partition    
    skip n s = S.drop (fromIntegral n) s

instance Filterable (Seq a) where
    filter = S.filter



    
