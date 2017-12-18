module Codec.Archive.Read where


import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign (Ptr)
import System.IO.Streams (Generator, InputStream)
import qualified System.IO.Streams as Stream


import Codec.Archive.Read.Internal
import Codec.Archive.Types


withArchive :: FilePath -> (InputStream Block -> IO a) -> IO a
withArchive filename' continue =
    bracket (openArchive filename') closeArchive
    (\archive -> Stream.fromGenerator (streamArchive archive) >>= continue)


streamArchive :: Ptr Archive -> Generator Block ()
streamArchive archive =
  do
    block <- liftIO $ getEntry archive
    case block of
      Nothing -> return ()
      Just entry ->
        do
          Stream.yield entry
          streamDataBlocks archive
          streamArchive archive


streamDataBlocks :: Ptr Archive -> Generator Block ()
streamDataBlocks archive =
  do
    block <- liftIO $ getDataBlock archive
    case block of
      Nothing -> return ()
      Just bytes' ->
        do
          Stream.yield bytes'
          streamDataBlocks archive
