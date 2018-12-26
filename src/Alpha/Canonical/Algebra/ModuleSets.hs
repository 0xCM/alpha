-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Algebra.ModuleSets
(
    GeneratingSet(..),
    SpanningSet(..),
    IndependentSet(..),
    BasisSet(..)

) where

import Alpha.Canonical.Relations
import Alpha.Canonical.Common.Asci

-- | A generating set G for a module M is the intersection of all 
-- submodules of M that contain G that is itself a submodule of M
newtype GeneratingSet r m = GeneratingSet [m]
    deriving(Generic)
instance Newtype (GeneratingSet r m)

type instance Individual (GeneratingSet r m) = m

newtype SpanningSet r m = SpanningSet [m]
    deriving(Generic)
instance Newtype (SpanningSet r m)

type instance Individual (SpanningSet r m) = m

newtype IndependentSet r m = IndependentSet [m]
    deriving(Generic)
instance Newtype (IndependentSet r m)

type instance Individual (IndependentSet r m) = m

newtype BasisSet r m = BasisSet [m]
    deriving(Generic)
instance Newtype (BasisSet r m)

type instance Individual (BasisSet r m) = m

setformat::(Formattable m) => [m] -> Text     
setformat s = enclose LBrace RBrace (format elements) where
    elements =  weave Comma (format <$> s)

-- instance DiscreteSubset (BasisSet r m)  l  where
--     subelements = unwrap
instance (Formattable r, Formattable m) => Formattable (BasisSet r m) where
    format (BasisSet x) = setformat x
instance (Formattable r, Formattable m) => Show (BasisSet r m) where
    show = string . format    
    
-- instance DiscreteSubset (SpanningSet r m)  l  where
--     subelements = unwrap
instance (Formattable r, Formattable m) => Formattable (SpanningSet r m) where
    format (SpanningSet x) = setformat x
instance (Formattable r, Formattable m) => Show (SpanningSet r m) where
    show = string . format
    
-- instance DiscreteSubset (IndependentSet r m)  l  where
--     subelements = unwrap
instance (Formattable r, Formattable m) => Formattable (IndependentSet r m) where
    format (IndependentSet x) = setformat x    
instance (Formattable r, Formattable m) => Show (IndependentSet r m) where
    show = string . format