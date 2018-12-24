module Alpha.Canonical.Common.Length
(
    Length(..),
    
) where
import Alpha.Canonical.Common.Root
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.ByteString as EG
import qualified Data.ByteString.Lazy as LZ

class Length a where    
    length::(Integral n) => a -> n

instance Length [a] where
    length x =  List.length x |> fromIntegral
instance Length Text where
    length t =   Text.length t |> fromIntegral    
instance Length Char where
    length c = 1

