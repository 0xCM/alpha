module Alpha.Text.Text where

--import Text.Show
import qualified Data.Text as T
import qualified Data.String as S
import Alpha.Canonical
import Alpha.Base

    
        
instance Cloneable Int Text where
    type Cloned Int Text = Text
    clone = T.replicate
    

instance Weavable Char Text where
    weave = T.intersperse
    