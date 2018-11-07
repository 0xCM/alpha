-----------------------------------------------------------------------------
-- | Defines file system model
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}

module Alpha.System.Files
(
    PathComponent(..),
    DriveLetter,
    folderName, FolderName(..),
    folder, FolderPath(..),
    extension, FileExtension(..),
    fileName, FileName(..),
    file, FilePath(..),
    relativeFile, RelativeFilePath(..),
    relativeFolder, RelativeFolderPath(..),
    size
)

where

import Control.Monad (filterM,forM)
import Control.Exception

import System.IO (IOMode(..), hClose, hFileSize, openFile, IO(..))

import System.Directory
import Data.Data
import GHC.Generics
import GHC.TypeLits
import Prelude(Show,($))
import qualified System.FilePath as FP
import Prelude(return)
import qualified Data.List as List
import Data.Text hiding (concat,length)
import Alpha.Text
import Alpha.Canonical
import Alpha.Base
import Alpha.Text.Char
import Alpha.Data.Maybe

class PathComponent a where
    path::a -> Text

newtype DriveLetter = DriveLetter Char
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

instance PathComponent DriveLetter where
    path (DriveLetter x) = concat x colon

newtype FolderName = FolderName Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

instance PathComponent FolderName where
    path (FolderName x) = x
        
newtype FolderPath = FolderPath Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

instance PathComponent FolderPath where
    path (FolderPath x) = x
    
newtype FileExtension = FileExtension Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

instance PathComponent FileExtension where
    path (FileExtension x) = x

newtype FileName = FileName Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

instance PathComponent FileName where
    path (FileName x) = x

newtype FilePath = FilePath Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

instance PathComponent FilePath where
    path (FilePath x) = x

newtype RelativeFilePath = RelativeFilePath Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

instance PathComponent RelativeFilePath where
    path (RelativeFilePath x) = x
        
newtype RelativeFolderPath = RelativeFolderPath Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

instance PathComponent RelativeFolderPath where
    path (RelativeFolderPath x) = x
    
instance Concatenable DriveLetter FolderName where
    type Concatenated DriveLetter FolderName = FolderPath
    concat x y = splat [path x, fslash, path y] |> folder

instance Concatenable DriveLetter RelativeFolderPath where
    type Concatenated DriveLetter RelativeFolderPath = FolderPath
    concat x y = splat [path x, fslash, path y] |> folder

instance Concatenable DriveLetter FileName where
    type Concatenated DriveLetter FileName = FilePath 
    concat x y = splat [path x, fslash, path y] |> file

instance Concatenable DriveLetter RelativeFilePath where
    type Concatenated DriveLetter RelativeFilePath = FilePath 
    concat x y = splat [path x, fslash, path y] |> file

instance Concatenable FolderPath FolderName where
    type Concatenated FolderPath FolderName = FolderPath
    concat x y = splat [path x, fslash, path y] |> folder
        
instance Concatenable FolderPath FileName where
    type Concatenated FolderPath FileName = FilePath
    concat x y = splat [path x, fslash, path y] |> file

instance Concatenable FolderPath RelativeFolderPath where
    type Concatenated FolderPath RelativeFolderPath = FolderPath
    concat x y = splat [path x, fslash, path y] |> folder

instance Concatenable FolderPath RelativeFilePath where
    type Concatenated FolderPath RelativeFilePath = FilePath
    concat x y = splat [path x, fslash, path y] |> file
            
instance Concatenable FileName FileExtension where
    type Concatenated FileName FileExtension = FilePath
    concat x y = splat [path x, dot, path y] |> file

-- / path + ext = path.ext
instance Concatenable FilePath FileExtension where
    type Concatenated FilePath FileExtension = FilePath
    concat x y = splat [path x, dot, path y] |> file
    
-- | ext1 + ext2 = ext1.ext2    
instance Concatenable FileExtension FileExtension where
    type Concatenated FileExtension FileExtension = FileExtension
    concat x y = splat [path x, dot, path y] |> extension

--type Extensionable = (# FileName | FilePath | RelativeFilePath #)    

-- | Constructs a 'DriveLetter'    
drive::Char -> DriveLetter
drive c = DriveLetter c

-- | Constructs a path to a folder
folder::Text -> FolderPath
folder x = FolderPath x

-- | Constructs a folder name
folderName :: Text -> FolderName
folderName x = FolderName x

-- | Constructs a relative folder path
relativeFolder :: Text -> RelativeFolderPath
relativeFolder x = RelativeFolderPath x

-- | Constructs a path to a file
file::Text -> FilePath
file x = FilePath x

-- | Constructs a relative file path
relativeFile::Text -> RelativeFilePath
relativeFile x = RelativeFilePath x

-- | Constructs a file name
fileName::Text -> FileName
fileName x  = FileName x

-- | Constructs a file extension
extension::Text -> FileExtension
extension x = FileExtension x

size :: FilePath -> IO Integer    
size (FilePath path) = 
     bracket (openFile (show path) ReadMode) hClose 
        (\h -> do 
            size <- hFileSize h 
            return size)        

getExtension':: Text -> Maybe FileExtension
getExtension' x = case (x |> rightOfLast dot) of 
        Just y -> extension y |> some
        _ -> none

