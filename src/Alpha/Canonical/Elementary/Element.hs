{-# LANGUAGE UndecidableInstances #-}

module Alpha.Canonical.Elementary.Element
(
    Element(..),
 
)
where
import Alpha.Base
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Sequence as Sequence

type family Element a
type instance Element [a] = a
type instance Element (Bag a) = a
type instance Element (Seq a) = a
type instance Element (Stream a) = a
type instance Element (Map a b) = (a,b)
type instance Element (Vector a) = a

