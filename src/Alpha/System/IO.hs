-----------------------------------------------------------------------------
-- | IO Utilities
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.System.IO 
(
    readLines,
    readBytes,
    isFile,files,
    isFolder,folders,dir,
    print, out, out'
)
where

import System.IO(print, putStr)
import System.IO.Unsafe
import System.Console.ANSI
import Control.Monad.Primitive
import qualified Prelude as P
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified System.Directory as D
import qualified Data.Text.IO as T

import qualified Alpha.Data.List as List
import Alpha.Base
import Alpha.Text.Combinators
import Alpha.Data.Numbers
import Alpha.Canonical
import Alpha.System.Files
import Alpha.Data.AppMessage
import Alpha.Text as Text

-- | Renders a line of text to standard out 
out :: Show s => s -> IO()
out s = print s

-- | Renders text to standard out
out' :: Show s => s -> IO()
out' s = putStr (show s)

    
-- | Reads the lines of text from a file
readLines::FilePath -> [Text]
readLines x = x |> show |> T.readFile |> shredIO |> lines

-- | Reads a file into a 'ByteString
readBytes::FilePath -> ByteString
readBytes x =  ByteString.readFile (show (path x) ) |> shredIO

-- | Determines whether a specified folder exists
isFolder::FolderPath -> Bool
isFolder (FolderPath x) = show x |> D.doesDirectoryExist |> shredIO

-- | Determines whether a specifed file exists
isFile::FilePath -> Bool
isFile (FilePath x) = show x |> D.doesFileExist |> shredIO

-- | Returns the files and folders contained in a specified parent folder              
dir::FolderPath -> [Text]
dir (FolderPath x) 
    = x |> unpack |> D.listDirectory |> shredIO 
        |> fmap (\y -> Text.splat([ x , fslash,  pack y ]))

-- | Returns the files that are contained in a specified parent folder
files::FolderPath -> [FilePath]    
files x = x |> dir 
            |> fmap file
            |> List.filter isFile 
            
-- | Returns the subfolders that are contained in a specified parent folder
folders::FolderPath -> [FolderPath]    
folders x = x |> dir
              |> fmap folder
              |> List.filter isFolder
                                        
instance Jailbreak IO a where
    escape = shredIO

instance (PrimBase m) => Jailbreak m a where
    escape x = x |> unsafeInlinePrim

--data Emitter = forall a.Emitter(AppMessage a -> IO())


log::AppMessage a -> IO()
log (AppMessage severity text _) = do
    setSGR [SetColor Foreground intensity color]
    text |> out
    setSGR [Reset]
    where (intensity, color) = case severity of
                    Trivia -> (Dull, Black)
                    Info  -> (Vivid, Green)
                    Warn -> (Vivid, Yellow)
                    Error -> (Vivid, Red)
                    Fatal -> (Vivid, Red)
            
