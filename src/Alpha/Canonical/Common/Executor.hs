module Alpha.Canonical.Common.Executor where

import qualified Data.Text as Text
import Alpha.Canonical.Common.Root


newtype ResultInfo = ResultInfo Text

cmdresult::Text -> Either l ResultInfo
cmdresult x = Right(ResultInfo x)

class (Show c) => Executor c s f where
    type Command c
    
    type Success s :: Type
    type Success s = ResultInfo

    type Failure f :: Type
    type Failure f = ResultInfo

    execute::Command c -> Either (Failure f) (Success s)


data Print = Print Text
    deriving(Show)

instance Executor Print Text Text where
    type Command Print = Print

    execute c = do
        cmdresult (show c |> Text.pack)