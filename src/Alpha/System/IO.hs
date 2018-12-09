-----------------------------------------------------------------------------
-- | IO Utilities
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.System.IO 
(
    readLines,
    readBytes,
    isFile,files,
    isFolder,folders,dir,
    print, out, out',
    shredIO,IO,
    HexLine(..),
    readHexFile,
    hexline,
    RefCell(..),
    Cellular(..)
)
where
import System.IO(print, putStr)
import System.IO.Unsafe
import System.Console.ANSI
import Control.Monad.Primitive
import Data.IORef

import qualified Prelude as P
import qualified Data.ByteString as ByteString
import qualified System.Directory as D
import qualified Data.Text.IO as T


import Alpha.Base
import Alpha.Text.Combinators
import Alpha.Canonical
import Alpha.System.Files
import Alpha.Data.Message
import Alpha.Text as Text

newtype HexLine = HexLine (Int, Text)
    deriving (Eq,Ord)

-- | Just say "no" to the monolithic imprisonment of IO
shredIO :: IO a -> a
shredIO = unsafeDupablePerformIO 

-- | Renders a showable to standard out 
out'::Show s => s -> IO()
out' s = print s

-- | Renders text to standard out
out::(ToString s) => s -> IO()
out s = P.putStr (string s)

-- | Renders a line-feed to standard out
endl::IO()
endl = P.putStrLn ""

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
            |> filter isFile 
            
-- | Returns the subfolders that are contained in a specified parent folder
folders::FolderPath -> [FolderPath]    
folders x = x |> dir
              |> fmap folder
              |> filter isFolder
                                        

log::Message a -> IO()
log (Message severity text _) = do
    setSGR [SetColor Foreground intensity color]
    text |> out'
    setSGR [Reset]
    where (intensity, color) = case severity of
                    Trivia -> (Dull, Black)
                    Info  -> (Vivid, Green)
                    Warn -> (Vivid, Yellow)
                    Error -> (Vivid, Red)
                    Fatal -> (Vivid, Red)        


instance Show HexLine where
    show (HexLine (i,t)) = show t
                
-- | Create a numbered line of hextext
hexline :: Int -> Text -> HexLine
hexline i t = HexLine (i,t)

-- | Reads a hextext file
readHexFile :: FilePath -> [HexLine]
readHexFile path 
    =   path  |> readLines |> mapi (\(i,t) -> hexline (i+1) t)                    

instance Jailbreak IO a where
    escape = shredIO

instance (PrimBase m) => Jailbreak m a where
    escape x = x |> unsafeInlinePrim                    


newtype RefCell a = RefCell (IORef a)

class Cellular a where
    createcell::a -> RefCell a
    createcell = RefCell . shredIO . newIORef

    readcell::RefCell a -> a
    readcell (RefCell x) = shredIO $ readIORef x

    writecell::RefCell a -> a -> ()
    writecell (RefCell x) y = shredIO $ writeIORef x y

    updatecell::RefCell a -> (a -> (a,b)) -> b
    updatecell (RefCell x) f = shredIO $ atomicModifyIORef' x f        