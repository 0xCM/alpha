{-# LANGUAGE DefaultSignatures #-}

module Alpha.Control.Executor where

import qualified Data.Text as Text
import Alpha.Base
import Alpha.Canonical
import Alpha.Text

-- type family ErrorInfo (t :: Type) :: Type where
--     ErrorInfo (f a) = a

newtype ResultInfo = ResultInfo Text

success::Text -> Either l ResultInfo
success x = Right(ResultInfo x)

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
        success (show c |> Text.pack)