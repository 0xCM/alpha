module Alpha.Text.String
(
    string, IsString(..)
)
where
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Numbers

import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.List as List


instance Length String where
    length = convert . List.length

instance ToString String where
    string = id
