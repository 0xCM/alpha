-----------------------------------------------------------------------------
-- 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Seq
(
    Sequence(..), fseq
)

where

import Alpha.Canonical.Elementary
import Alpha.Canonical.Collective.Container
import Alpha.Canonical.Collective.Map

import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text


-- | Represents a sequence of indexed terms
newtype Sequence i t = Sequence (Map i (IndexedTerm i t))
    deriving(Eq, Ord, Generic)

    --deriving(Eq,Eq2,Ord,Ord2,Functor,Foldable,Traversable,Semigroup,Monoid,NFData,Generic,Data,Typeable)
instance Newtype(Sequence i a)

type instance Individual (Sequence i t) = IndexedTerm i t
type instance IndexedElement i (Sequence i t) = Individual (Sequence i t)

-- | Constructs a finite sequence
fseq::(Integral i) => [IndexedTerm i t] -> Sequence i t
fseq terms = (\t -> (termix t, t)) <$> terms |> Map.fromList |> Sequence
    

-- | Constructs an infinite sequence from via a supplied function 
-- defined on the domain of natural numbers   
-- iseq::(F1 Natural a) -> InfiniteSeq a
-- iseq f = InfiniteSeq [ SeqTerm (i, f i) | i <- [1..]]

-- instance Concatenable (Seq a) (Seq a) where
--     type Concatenated (Seq a) (Seq a) = Seq a   
--     concat a b = a <> b

-- instance Headed (Seq a) where    
--     head s = Seq.index s 0
--     tail s = snd $ Seq.splitAt 1 s
    
-- instance Container (Seq a) where
--     contain x = Seq.fromList x
--     contents = toList    
    
-- instance Queryable (Seq a) where
--     filter pred source =  toList source |> List.filter pred

-- instance Predicative (Seq a)  where
--     split = Seq.partition    
--     while = Seq.takeWhileL 

-- instance  Paged (Seq a) where    
--     take n s = Seq.take (fromIntegral n) s
--     skip n s = Seq.drop (fromIntegral n) s
--     splitAt i s = Seq.splitAt (fromIntegral i) s
    
-- instance Mappable (Seq a) a b where
--     type Mapped (Seq a) a b = Seq b
--     map = fmap
                        
-- instance Sequential (Seq a)    

-- instance Singletary (Seq a) where
--     singleton x = contain [x]

instance (Integral i) => Unionizable (Sequence i a) where
    union x y = wrap $ Map.union (unwrap x) (unwrap y)
    
instance (Integral i) => IsList (Sequence i a) where
    type Item (Sequence i a) = IndexedTerm i a
    toList (Sequence terms) = associated terms
    fromList l = fseq l
    
instance (Integral i) => Indexed i (Sequence i a) where
    at (Sequence map) i = map Map.! i
    
instance Integral i => Discrete (Sequence i a) where
    individuals (Sequence terms) = (\x -> snd x) <$> (terms |> toList)
        
instance  (Integral i) => Vacuous (Sequence i a) where
    null (Sequence m) = (Map.null m)
    empty = fromList []

instance  (Integral i) => Singletary (Sequence i a) where
    singleton term = fseq [term]

sorted::(Ord i) => Sequence i a -> [(i,IndexedTerm i a)]
sorted = (List.sortOn fst) . toList . unwrap

instance Ord i => Paged (Sequence i a) where    
    take n s = s |> sorted |> List.take (fromIntegral n) |> fromList |> wrap
    skip n s = s |> sorted |> List.drop (fromIntegral n) |> fromList |> wrap
    splitAt n s = s |> sorted |> List.splitAt (fromIntegral n) 
                              |> (\(x,y) -> (fromList x |> wrap, fromList y |> wrap))

-- instance Ord i => Functor (Sequence i) where
--     fmap f (Sequence map) = map |> toList |> (<$>)(\(i,x) -> (i, f <$> x)) |> fromList |> Sequence

instance (Ord i, Formattable i, Formattable a) => Formattable (Sequence i a) where
    format (Sequence s) =  list s |> (<$>) format |> embrace

instance (Ord i, Formattable i, Formattable a) => Show (Sequence i a) where
    show = string . format
    
-- instance Ord i => Associated (Sequence i a) where
--     associates (Sequence s) = list s        