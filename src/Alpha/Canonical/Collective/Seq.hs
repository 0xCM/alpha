-----------------------------------------------------------------------------
-- 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Seq where

import Alpha.Canonical.Elementary
import Alpha.Canonical.Collective.Container

import qualified Data.Sequence as Seq

type instance Concatenated (Seq a) (Seq a) = Seq a   
type instance Individual (Seq a) = a

instance Concatenable (Seq a) (Seq a) where
    concat a b = a <> b

instance Headed (Seq a) where    
    head s = Seq.index s 0
    tail s = snd $ Seq.splitAt 1 s
    
instance Container (Seq a) where
    contain x = Seq.fromList x
    contents = toList    
    
instance Filterable (Seq a) where
    filter = Seq.filter

instance Predicative (Seq a)  where
    split = Seq.partition    
    while = Seq.takeWhileL 

instance  Paged (Seq a) where    
    take n s = Seq.take (fromIntegral n) s
    skip n s = Seq.drop (fromIntegral n) s
    splitAt i s = Seq.splitAt (fromIntegral i) s
    
instance Mappable (Seq a) a b where
    type Mapped (Seq a) a b = Seq b
    map = fmap
                        
instance Sequential (Seq a)    