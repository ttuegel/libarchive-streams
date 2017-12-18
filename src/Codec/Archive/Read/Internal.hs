{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Archive.Read.Internal where

import Control.Exception
import qualified Data.ByteString as ByteString
import Data.Typeable
import Foreign
import Foreign.C
import Prelude hiding (length)
import System.Posix.Files
import System.Posix.Types


import Codec.Archive.Types


data Archive

data Entry


foreign import ccall "archive.h archive_read_new"
    archive_read_new :: IO (Ptr Archive)

foreign import ccall "archive.h archive_read_free"
    archive_read_free :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_read_support_filter_all"
    archive_read_support_filter_all :: Ptr Archive -> IO ()

foreign import ccall "archive.h archive_read_support_format_all"
    archive_read_support_format_all :: Ptr Archive -> IO ()

foreign import ccall "archive.h archive_read_open_filename"
    archive_read_open_filename :: Ptr Archive -> CString -> CSize -> IO CInt

foreign import ccall "archive.h archive_read_next_header"
    archive_read_next_header :: Ptr Archive -> Ptr (Ptr Entry) -> IO CInt

foreign import ccall "archive.h archive_entry_pathname"
    archive_entry_pathname :: Ptr Entry -> IO CString

foreign import ccall "archive.h archive_entry_size"
    archive_entry_size :: Ptr Entry -> IO ByteCount

foreign import ccall "archive.h archive_entry_mode"
    archive_entry_mode :: Ptr Entry -> IO FileMode

foreign import ccall "archive.h archive_entry_hardlink"
    archive_entry_hardlink :: Ptr Entry -> IO CString

foreign import ccall "archive.h archive_entry_symlink"
    archive_entry_symlink :: Ptr Entry -> IO CString

foreign import ccall "archive.h archive_entry_nlink"
    archive_entry_nlink :: Ptr Entry -> IO LinkCount

foreign import ccall "archive.h archive_entry_uid"
    archive_entry_uid :: Ptr Entry -> IO UserID

foreign import ccall "archive.h archive_entry_gid"
    archive_entry_gid :: Ptr Entry -> IO GroupID

foreign import ccall "archive.h archive_entry_dev"
    archive_entry_dev :: Ptr Entry -> IO DeviceID

foreign import ccall "archive.h archive_entry_ino64"
    archive_entry_ino64 :: Ptr Entry -> IO FileID

foreign import ccall "archive.h archive_entry_rdevmajor"
    archive_entry_rdevmajor :: Ptr Entry -> IO DeviceID

foreign import ccall "archive.h archive_entry_rdevminor"
    archive_entry_rdevminor :: Ptr Entry -> IO DeviceID

foreign import ccall "archive.h archive_entry_atime"
    archive_entry_atime :: Ptr Entry -> IO EpochTime

foreign import ccall "archive.h archive_entry_ctime"
    archive_entry_ctime :: Ptr Entry -> IO EpochTime

foreign import ccall "archive.h archive_entry_mtime"
    archive_entry_mtime :: Ptr Entry -> IO EpochTime

foreign import ccall "archive.h archive_read_data_block"
    archive_read_data_block
      :: Ptr Archive -> Ptr CString -> Ptr CSize -> Ptr COff -> IO CInt


openArchive :: FilePath -> IO (Ptr Archive)
openArchive path = do
    p <- archive_read_new
    archive_read_support_filter_all p
    archive_read_support_format_all p
    r <-
        withCString path $ \cpath ->
        archive_read_open_filename p cpath (64 * 1024)
    case status r of
      OK -> return p
      _ -> throwArchiveException p

closeArchive :: Ptr Archive -> IO ()
closeArchive archive =
  do
    r <- archive_read_free archive
    case status r of
      OK -> return ()
      _ -> throwArchiveException archive

getEntry :: Ptr Archive -> IO (Maybe Block)
getEntry archive =
  alloca $ \pentry -> do
    r <- archive_read_next_header archive pentry
    case status r of
      EOF -> return Nothing
      OK ->
        do
          entry <- peek pentry
          filename <- archive_entry_pathname entry >>= peekCString
          mode <- archive_entry_mode entry
          nlink <- archive_entry_nlink entry
          let
            isHardlink = nlink > 1
          hardlink <-
              if isHardlink
              then Just <$> (archive_entry_hardlink entry >>= peekCString)
              else return Nothing
          let
            isSymlink = (mode .&. symbolicLinkMode) > 0
          symlink <-
              if isSymlink
              then Just <$> (archive_entry_symlink entry >>= peekCString)
              else return Nothing
          uid <- archive_entry_uid entry
          gid <- archive_entry_gid entry
          size <- archive_entry_size entry
          dev <- archive_entry_dev entry
          ino <- archive_entry_ino64 entry
          rdevmajor <- archive_entry_rdevmajor entry
          rdevminor <- archive_entry_rdevminor entry
          let rdev = (rdevmajor, rdevminor)
          atime <- archive_entry_atime entry
          ctime <- archive_entry_ctime entry
          mtime <- archive_entry_mtime entry
          return $ Just FileHeader {..}
      _ -> throwArchiveException archive


getDataBlock :: Ptr Archive -> IO (Maybe Block)
getDataBlock archive =
  alloca $ \pbuffer ->
  alloca $ \plength ->
  alloca $ \poffset ->
    do
      r <- archive_read_data_block archive pbuffer plength poffset
      case status r of
        EOF -> return Nothing
        OK ->
          do
            length <- peek plength
            offset <- peek poffset
            buffer <- peek pbuffer
            bytes <- ByteString.packCStringLen (buffer, fromIntegral length)
            return $ Just DataBlock {..}
        _ -> throwArchiveException archive


-- * Exceptions

data Status = EOF | OK | RETRY | WARN | FAILED | FATAL

status :: CInt -> Status
status 1 = EOF
status 0 = OK
status (-10) = RETRY
status (-20) = WARN
status (-25) = FAILED
status _ = FATAL


data ArchiveException = ArchiveException String
  deriving (Show, Typeable)

instance Exception ArchiveException


checkArchiveError :: Ptr Archive -> CInt -> IO Bool
checkArchiveError archive code
    | code >= 0 = return $ code == 1
    | otherwise = throwArchiveException archive


foreign import ccall "archive.h archive_error_string"
    archiveErrorString :: Ptr Archive -> IO CString

throwArchiveException :: Ptr Archive -> IO a
throwArchiveException archive = do
    pstr <- archiveErrorString archive
    str <- peekCString pstr
    throw $ ArchiveException str
