module Alpha.Canonical.Algebra.Structure
(
    StructureOperator(..)
)
where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Relations
import Alpha.Canonical.Functions

class (Operator 2 f a) => StructureOperator f a where
    