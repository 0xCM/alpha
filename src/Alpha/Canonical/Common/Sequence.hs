-----------------------------------------------------------------------------
-- 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Sequence
(
    Sequence(..), sequence,
) where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Indexing
import Alpha.Canonical.Common.Setwise
import Alpha.Canonical.Common.Sequential

import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector


-- | Represents a sequence of indexed terms
newtype Sequence i t = Sequence (Map i (IxTerm i t))
    deriving(Eq, Ord, Generic)    
instance Newtype(Sequence i a)

type instance Individual (Sequence i t) = IxTerm i t

-- | Constructs finite sequence
sequence::(Integral i) => [IxTerm i t] -> Sequence i t
sequence terms = (\t -> (termix t, t)) <$> terms |> Map.fromList |> Sequence

seqsort::(Ord i) => Sequence i a -> [(i,IxTerm i a)]
seqsort = (List.sortOn fst) . toList . unwrap

instance (Integral i) => IsList (Sequence i a) where
    type Item (Sequence i a) = IxTerm i a
    toList (Sequence terms) = associated terms
    fromList l = sequence l
    
instance (Integral i) => Indexable (Sequence i a) where
    type Indexer (Sequence i a) = i
    idx (Sequence map) i = map Map.! i
    
instance Integral i => Discrete (Sequence i a) where
    individuals (Sequence terms) = (\x -> snd x) <$> (terms |> toList)
        
instance  (Integral i) => Nullity (Sequence i a) where
    null (Sequence m) = (Map.null m)
    empty = fromList []

instance  (Integral i) => Singleton (Sequence i a) where
    singleton term = sequence [term]

instance Ord i => Paged (Sequence i a) where    
    take n s = s |> seqsort |> List.take (fromIntegral n) |> fromList |> wrap
    skip n s = s |> seqsort |> List.drop (fromIntegral n) |> fromList |> wrap
    splitAt n s = s |> seqsort |> List.splitAt (fromIntegral n) 
                              |> (\(x,y) -> (fromList x |> wrap, fromList y |> wrap))

instance (Ord i, Formattable i, Formattable a) => Formattable (Sequence i a) where
    format (Sequence s) =  list s |> (<$>) format |> embrace

instance (Ord i, Formattable i, Formattable a) => Show (Sequence i a) where
    show = string . format
    
