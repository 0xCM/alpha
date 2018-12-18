module Alpha.System.Log
(
    applog
)
where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical
import Alpha.System.IO
import Alpha.Canonical.Text.Asci
import Alpha.Data.Message
import System.Console.ANSI
import qualified System.Console.ANSI as ANSI
import Prelude(putStrLn,putStr)

log'::Message a -> IO()
log' (Message severity text _) = do
    setSGR [SetColor Foreground intensity color]
    text |> enclose prefix suffix |> string |> putStrLn
    setSGR [Reset]
    where (intensity, color, prefix, suffix) 
                = case severity of
                    Trivia -> (Dull, Cyan, Empty,Empty)
                    Info  -> (Vivid, Green, Empty,Empty)
                    Warn -> (Vivid, Yellow, Empty,Empty)
                    Error -> (Vivid, Red, Empty,Empty)
                    Fatal -> (Vivid, Red, Bang, Bang) 

applog::Message a -> IO()
applog =  log'             
    
