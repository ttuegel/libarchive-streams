module Codec.Archive.Types where


import Data.ByteString (ByteString)
import Prelude hiding (length)
import System.Posix.Types


data Block
    = FileHeader
      {
        filename :: !FilePath,
        hardlink :: !(Maybe FilePath),
        symlink :: !(Maybe FilePath),
        uid :: !UserID,
        gid :: !GroupID,
        mode :: !FileMode,
        size :: !ByteCount,
        dev :: !DeviceID,
        ino :: !FileID,
        rdev :: !(DeviceID, DeviceID),
        nlink :: !LinkCount,
        atime :: !EpochTime,
        ctime :: !EpochTime,
        mtime :: !EpochTime
      }
    | DataBlock
      {
        length :: !ByteCount,
        offset :: !FileOffset,
        bytes :: !ByteString
      }
