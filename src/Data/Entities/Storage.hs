module Data.Entities.Storage(hash, newFile) where

import Codec.Archive.Tar qualified as TAR
import Codec.Compression.GZip qualified as GZip
import Control.Monad.Reader
import Data.Binary qualified as BN
import Data.ByteString qualified as BS
import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.ContentType qualified as CT
import Data.Digest.CityHash
import Data.Entities.Internal
import Data.Functor ((<&>))
import Data.Gitignore
import Data.Text (Text (), unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import System.Directory
import System.Storage (StorageError, StorageMonad)
import System.Storage qualified as ST

hash :: BS.ByteString -> Text
hash =
  format . TE.decodeUtf8 . BS.toStrict . B64.encode . BN.encode . cityHash128
  where
    -- cityHash takes strict bytestrings since it's foreign, which
    -- is unfortunate.

    format = trim . T.replace "/" "-"
    trim s = T.take (T.length s - 2) s

-- | Ensure file is on the storage, returning a curried entity constructor.
-- Doesn't check whether the file exists and errors if it doesn't.
newFile ::
  (StorageMonad m) =>
  Text ->
  FilePath ->
  m (Either StorageError (UTCTime -> VCRootId -> File))
newFile t p = do
  isDir <- liftIO $ doesDirectoryExist p
  (dat, ct) <- if isDir then readDir p else readFile' p

  -- large files won't fit in memory - dat is lazy, but
  -- hash operation isn't. requires testing.
  let h = hash $ BS.toStrict dat
  destPath <- ST.expandPath (unpack h)

  -- TODO: move to logger
  liftIO $ putStrLn $ "content type: " ++ CT.unpack ct
  liftIO $ putStrLn $ "path: " ++ destPath
  (File t h ct <$) <$> ST.writeFile (unpack h) dat
  where
    readDir dir =
      liftIO $
        listDirectory dir
          >>= tryGitignore
          >>= TAR.pack dir
          <&> GZip.compress . TAR.write
          <&> (,"application/gzip")

    readFile' file = do
      d <- liftIO $ BL.readFile file
      ct <- liftIO $ CT.deduce file
      return (d, ct)

    -- fs isn't the full file tree -
    -- some gitignore entries will be ignored!
    tryGitignore :: [FilePath] -> IO [FilePath]
    tryGitignore fs
      | ".gitignore" `elem` fs = do
          putStrLn ".gitignore found"
          c <- lines <$> readFile ".gitignore"
          return $ applyGitignore c fs
      | otherwise = return fs
