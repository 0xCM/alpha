module Alpha.Text.Char where
import Data.Char(Char)
import qualified Data.Text as T
import Alpha.Canonical

instance Length Char where
    length c = 1

instance ToString Char where
    string x = [x]

instance Formattable Char where
    format x = T.singleton x

    