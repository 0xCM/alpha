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
type instance Element (Seq a) = a

instance (Eq a) => Structure (Seq a) where
    type Individual (Seq a) = Element (Seq a)

instance Concatenable (Seq a) (Seq a) where
    concat a b = a <> b


instance (Eq a) => Headed (Seq a) where    
    head s = Seq.index s 0
    tail s = snd $ Seq.splitAt 1 s
    
instance Container (Seq a) where
    contain x = Seq.fromList x
    contents = toList    
    
instance Filterable (Seq a) where
    filter = Seq.filter
        
instance (Eq a) => Sequential (Seq a) where    
    take n s = Seq.take (fromIntegral n) s
    split = Seq.partition    
    skip n s = Seq.drop (fromIntegral n) s
    splitAt i s = Seq.splitAt (fromIntegral i) s
    while = Seq.takeWhileL 
    
instance Mappable (Seq a) a b where
    type Mapped (Seq a) a b = Seq b
    map = fmap
                        