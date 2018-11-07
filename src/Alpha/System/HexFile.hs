-----------------------------------------------------------------------------
-- | Operations and types related to file-based hex representation
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.System.HexFile where
import qualified Data.ByteString as ByteString

import Alpha.Base
import Alpha.System.Files
import Alpha.Text
import Alpha.Canonical
import Alpha.System.Files
import Alpha.System.IO
import Alpha.Data.List(mapi)

newtype HexLine = HexLine (Int, Text)
    deriving (Eq,Ord)

instance Show HexLine where
    show (HexLine (i,t)) = show t

-- | Create a numbered line of hextext
hexline :: Int -> Text -> HexLine
hexline i t = HexLine (i,t)

-- | Reads a hextext file
readHexFile :: FilePath -> [HexLine]
readHexFile path 
    =   path  |> readLines |> mapi (\(i,t) -> hexline (i+1) t)