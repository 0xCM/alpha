-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.System.FilePath
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
)

where
import Alpha.Canonical.Common.Asci
import Alpha.Canonical.Common.TextUtil(rightOfLast)
import Alpha.Canonical.Relations
import qualified Data.Text as Text

-- | Represents some part of a path, i.e. a full path, a folder name,
-- a relative path, etc.
class PathComponent a where
    path::a -> Text

data DriveChar =
    A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving(Eq, Ord, Show, Generic, Data, Typeable)

instance Formattable DriveChar where
    format c = show c |> text

instance Length DriveChar where
    length _ = 1
    
-- | Defines a drive letter    
newtype DriveLetter = DriveLetter DriveChar
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

-- | Defines the simple name of a folder, i.e. a folder name sans path 
newtype FolderName = FolderName Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)
            
-- | Represents a path to a folder    
newtype FolderPath = FolderPath Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

-- | Represents a file extension containing one or more segments    
newtype FileExtension = FileExtension Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

-- | Defines the name of a file, with or without extension, but sans path 
newtype FileName = FileName Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)

-- | Defines the full path of a file
newtype FilePath = FilePath Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)
    
-- | Defines a path segment, relative to some location, containing a filename
newtype RelativeFilePath = RelativeFilePath Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)
        
-- | Defines a path segment, relative to some location, containing a folder name
newtype RelativeFolderPath = RelativeFolderPath Text
    deriving(Eq, Ord, Show, Generic, Data, Typeable, Formattable, Length)


-- | Constructs a 'DriveLetter'    
drive::DriveChar -> DriveLetter
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

getExtension':: Text -> Maybe FileExtension
getExtension' x = case (x |> rightOfLast Period) of 
        Just y -> extension y |> just
        _ -> none
        
instance PathComponent DriveLetter where
    path (DriveLetter x) = (format x ) <>  Colon
instance PathComponent FilePath where
    path (FilePath x) = x
instance PathComponent FolderPath where
    path (FolderPath x) = x    
instance PathComponent FileExtension where
    path (FileExtension x) = x
instance PathComponent FolderName where
    path (FolderName x) = x    
instance PathComponent FileName where
    path (FileName x) = x
instance PathComponent RelativeFilePath where
    path (RelativeFilePath x) = x
instance PathComponent RelativeFolderPath where
    path (RelativeFolderPath x) = x
    
instance BiConcatenable DriveLetter FolderName where
    type BiConcatenated DriveLetter FolderName = FolderPath
    biconcat x y = Text.concat [path x, FSlash, path y] |> folder

instance BiConcatenable DriveLetter RelativeFolderPath where
    type BiConcatenated DriveLetter RelativeFolderPath = FolderPath
    biconcat x y = Text.concat [path x, FSlash, path y] |> folder

instance BiConcatenable DriveLetter FileName where
    type BiConcatenated DriveLetter FileName = FilePath 
    biconcat x y = Text.concat [path x, FSlash, path y] |> file

instance BiConcatenable DriveLetter RelativeFilePath where
    type BiConcatenated DriveLetter RelativeFilePath = FilePath 
    biconcat x y = Text.concat [path x, FSlash, path y] |> file

instance BiConcatenable FolderPath FolderName where
    type BiConcatenated FolderPath FolderName = FolderPath
    biconcat x y = Text.concat [path x, FSlash, path y] |> folder        

instance BiConcatenable FolderPath FileName where
    type BiConcatenated FolderPath FileName = FilePath
    biconcat x y = Text.concat [path x, FSlash, path y] |> file

instance BiConcatenable FolderPath RelativeFolderPath where
    type BiConcatenated FolderPath RelativeFolderPath = FolderPath
    biconcat x y = Text.concat [path x, FSlash, path y] |> folder

instance BiConcatenable FolderPath RelativeFilePath where
    type BiConcatenated FolderPath RelativeFilePath = FilePath
    biconcat x y = Text.concat [path x, FSlash, path y] |> file            

instance BiConcatenable FileName FileExtension where
    type BiConcatenated FileName FileExtension = FilePath
    biconcat x y = Text.concat [path x, Period, path y] |> file
    
instance BiConcatenable FilePath FileExtension where
    type BiConcatenated FilePath FileExtension = FilePath
    biconcat x y = Text.concat [path x, Period, path y] |> file    

instance BiConcatenable FileExtension FileExtension where
    type BiConcatenated FileExtension FileExtension = FileExtension
    biconcat x y = Text.concat [path x, Period, path y] |> extension    