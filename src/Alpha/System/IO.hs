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
    isFile,
    files,
    isFolder,
    folders,
    dir,
    filesize,
    HexLine(..),
    readHexFile,
    hexline,
    out,
)
where

import Alpha.Base
import Alpha.Native
import Alpha.Text.Combinators
import Alpha.Canonical
import Alpha.System.FilePath
import Alpha.Text as Text

import System.Console.ANSI
import qualified Prelude as P
import qualified System.Directory as Dir

newtype HexLine = HexLine (Int, Text)
    deriving (Eq,Ord)

class Identifier a where
    type Identity a
    identity::a -> Identity a    

class Source a where
    type Emitted a
    read::Identity a -> s -> Emitted a

class Target a where
    type Received a
    type Receipt a        
    write::Received a -> a -> Receipt a
    

filesize' :: FilePath -> IO Integer    
filesize' (FilePath path) = 
        bracket (openFile (show path) ReadMode) hClose 
        (\h -> do 
            size <- hFileSize h 
            return size)        
    
filesize::FilePath -> Integer
filesize = shredIO . filesize'
    
-- | Renders text to standard out
out::(ToString s) => s -> ()
out s = P.putStr (string s) |> shredIO

-- | Reads the lines of text from a file
readLines::FilePath -> [Text]
readLines x = show x |> readFileText' |> shredIO |> lines

-- | Reads a file into a 'ByteString
readBytes::FilePath -> ByteString
readBytes x =  show x |> readFileBytes' |> shredIO

-- | Determines whether a specified folder exists
isFolder::FolderPath -> Bool
isFolder (FolderPath x) = show x |> Dir.doesDirectoryExist |> shredIO

-- | Determines whether a specifed file exists
isFile::FilePath -> Bool
isFile (FilePath x) = show x |> Dir.doesFileExist |> shredIO

-- | Returns the files and folders contained in a specified parent folder              
dir::FolderPath -> [Text]
dir (FolderPath x) 
    = x |> unpack |> Dir.listDirectory |> shredIO 
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

-- | Create a numbered line of hextext
hexline :: Int -> Text -> HexLine
hexline i t = HexLine (i,t)

-- | Reads a hextext file
readHexFile :: FilePath -> [HexLine]
readHexFile path 
    =   path  |> readLines |> mapi (\(i,t) -> hexline (i+1) t)                    
              

instance Show HexLine where
    show (HexLine (i,t)) = show t
                    
