-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Common.FiniteSeq
(
    module X,
    FiniteSeq(..),
    finiteseq,

) where
import Alpha.Canonical.Common.Root as X
import Alpha.Canonical.Common.Indexing as X
import Alpha.Canonical.Common.Setwise as X
import Alpha.Canonical.Common.Sequential as X

import qualified Data.Vector as Vector
    
newtype FiniteSeq a = FiniteSeq (Vector a)
    deriving (Eq,Ord,Generic,Data,Typeable)
    deriving (Functor, Foldable, Traversable, Applicative, Monad, Semigroup)
    deriving (Container)
instance Newtype (FiniteSeq a)

type instance Individual (FiniteSeq a) = a

finiteseq::[a] -> FiniteSeq a
finiteseq terms = FiniteSeq $ Vector.fromList terms

-- *FiniteSeq membership
-------------------------------------------------------------------------------
instance Indexable (FiniteSeq a) where
    type Indexer (FiniteSeq a) = Int
    (FiniteSeq s) !! i = s !! i

instance IntMappable (FiniteSeq a) (FiniteSeq b) where    
    imap f src = wrap $ Vector.imap f (unwrap src)
        