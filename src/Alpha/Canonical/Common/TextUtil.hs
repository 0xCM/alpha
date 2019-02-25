-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.TextUtil
(    
)
 where
import Alpha.Base
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Asci
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Encode.Pretty

import qualified Data.Text as Text
import qualified Data.List as List
import Numeric(showIntAtBase)

