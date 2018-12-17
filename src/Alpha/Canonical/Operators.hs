-----------------------------------------------------------------------------
-- | Defines predicate operators and types
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Operators
(    
    module X,
    Faceted(..),FacetValue(..), facetVal,
    Computable(..),

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Element
import Alpha.Canonical.Operators.Logical as X
import Alpha.Canonical.Operators.Functions as X
import Alpha.Canonical.Operators.Operative as X

import qualified Data.Stream.Infinite as Stream
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

    
class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  symstr @f |> Text.pack
    
data FacetValue f v = FacetValue v

facetVal::(Faceted f v) => v -> FacetValue f v
facetVal val = FacetValue val    

instance Faceted "length" Int where

-- | Characterizes a deferred computation or a computation
-- specification    
class Computable a where
    -- | The type of computed value
    type Computed a

    -- | Effects the computation
    compute::a -> Computed a
