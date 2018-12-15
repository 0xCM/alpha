-----------------------------------------------------------------------------
-- | Defines file path vocabulary
-- Copyright   :  (c) 0xCM, 2018
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
import Alpha.Text
import Alpha.Canonical
import Alpha.Base



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

type instance Concatenated DriveLetter FolderName = FolderPath
type instance Concatenated DriveLetter RelativeFolderPath = FolderPath
type instance Concatenated DriveLetter FileName = FilePath 
type instance Concatenated DriveLetter RelativeFilePath = FilePath 
type instance Concatenated FolderPath FolderName = FolderPath
type instance Concatenated FolderPath FileName = FilePath
type instance Concatenated FolderPath RelativeFolderPath = FolderPath
type instance Concatenated FolderPath RelativeFilePath = FilePath
type instance Concatenated FileName FileExtension = FilePath
type instance Concatenated FilePath FileExtension = FilePath
type instance Concatenated FileExtension FileExtension = FileExtension

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
getExtension' x = case (x |> rightOfLast dot) of 
        Just y -> extension y |> just
        _ -> none

instance PathComponent DriveLetter where
    path (DriveLetter x) = (format x ) <>  colon        
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

instance Appendable DriveLetter FolderName where
    append x y = splat [path x, fslash, path y] |> folder
instance Appendable DriveLetter RelativeFolderPath where
    append x y = splat [path x, fslash, path y] |> folder
instance Appendable DriveLetter FileName where
    append x y = splat [path x, fslash, path y] |> file
instance Appendable DriveLetter RelativeFilePath where
    append x y = splat [path x, fslash, path y] |> file
instance Appendable FolderPath FolderName where
    append x y = splat [path x, fslash, path y] |> folder        
instance Appendable FolderPath FileName where
    append x y = splat [path x, fslash, path y] |> file
instance Appendable FolderPath RelativeFolderPath where
    append x y = splat [path x, fslash, path y] |> folder
instance Appendable FolderPath RelativeFilePath where
    append x y = splat [path x, fslash, path y] |> file            
instance Appendable FileName FileExtension where
    append x y = splat [path x, dot, path y] |> file
instance Appendable FilePath FileExtension where
    append x y = splat [path x, dot, path y] |> file    
instance Appendable FileExtension FileExtension where
    append x y = splat [path x, dot, path y] |> extension    