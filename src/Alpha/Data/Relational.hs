module Alpha.Data.Relational where
import Alpha.Base
import Alpha.Data.Product

class Projector a b where
    project::a -> b

