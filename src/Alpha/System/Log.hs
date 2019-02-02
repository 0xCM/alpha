-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.System.Log
(
    applog
)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Common.Asci
import Alpha.System.IO
import Alpha.System.Message

import System.Console.ANSI
import qualified System.Console.ANSI as ANSI
import Prelude(putStrLn,putStr)

applog::Message a -> IO()
applog (Message severity text _) = do
    setSGR [SetColor Foreground intensity color]
    text |> fence prefix suffix |> string |> putStrLn
    setSGR [Reset]
    where (intensity, color, prefix, suffix) 
                = case severity of
                    Trivia -> (Dull, Cyan, Empty,Empty)
                    Info  -> (Vivid, Green, Empty,Empty)
                    Warn -> (Vivid, Yellow, Empty,Empty)
                    Error -> (Vivid, Red, Empty,Empty)
                    Fatal -> (Vivid, Red, Bang, Bang) 

