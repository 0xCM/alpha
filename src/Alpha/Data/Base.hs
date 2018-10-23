-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Data.Base
(
    module X,
    String,
    Text,
    Set,
    HashSet,
    HashMap,
    Either(..),
    IsString,
    Ord,
    Bool(..),(&&), (||), not, otherwise
    

)
where
import Data.Bits as X
import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Char as X
import Data.Data as X
import Data.Set(Set)
import Data.Default as X
import Data.Kind as X
import Data.Either.Combinators as X
import Data.Either(Either(..))
import Data.Eq as X
import qualified Data.Foldable as X hiding(concat,or) 
import Data.Int as X
import Data.Ix as X
import Data.Maybe as X
import Data.Ord(Ord)
import Data.String(String,IsString)
import Data.Text(Text)
import Data.Traversable as X
import Data.Typeable as X
import Data.Word as X
import Data.HashSet(HashSet)
import Data.HashMap.Strict(HashMap)

