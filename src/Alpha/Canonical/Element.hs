module Alpha.Canonical.Element(Element(..))
where
import Alpha.Base

type family Element a
type instance Element (Set a) = a
type instance Element [a] = a
type instance Element (Bag a) = a
type instance Element (Seq a) = a
type instance Element (Stream a) = a
type instance Element (Map a b) = (a,b)
type instance Element (Vector a) = a



