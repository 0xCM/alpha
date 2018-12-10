module Alpha.System.Log
(
    logmsg
)
where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical
import Alpha.System.IO
import Alpha.Data.Message
import System.Console.ANSI

log'::Message a -> IO()
log' (Message severity text _) = do
    setSGR [SetColor Foreground intensity color]
    text |> out'
    setSGR [Reset]
    where (intensity, color) = case severity of
                    Trivia -> (Dull, Black)
                    Info  -> (Vivid, Green)
                    Warn -> (Vivid, Yellow)
                    Error -> (Vivid, Red)
                    Fatal -> (Vivid, Red) 

logmsg::Message a -> ()
logmsg = shredIO . log'             
    