module Data.Entities where

import Codec.Archive.Tar qualified as TAR
import Codec.Compression.GZip qualified as GZip
import Control.Monad.Reader
import Data.Binary qualified as BN
import Data.ByteString qualified as BS
import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.ContentType (ContentType ())
import Data.ContentType qualified as CT
import Data.Digest.CityHash
import Data.Text (Text (), unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import System.Directory
import System.Storage as ST

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Verse
  title Text
  content Text
  file (Maybe FileId)
  time UTCTime
  root VCRootId
Page
  title Text
  verses [VerseId]
  time UTCTime
  root VCRootId
File
  title Text
  name Text
  type ContentType
  time UTCTime
  root VCRootId
  UniqueFName name
VCRoot
|]

class HasTitle a where
  title :: a -> Text

instance HasTitle a => HasTitle (Entity a) where
  title = title . entityVal

instance HasTitle Verse where
  title = verseTitle

instance HasTitle File where
  title = fileTitle

instance HasTitle Page where
  title = pageTitle

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

  -- large files won't fit in memory - dat is lazy, but
  -- hash operation isn't. requires testing.
  (dat, ct) <-
    if isDir
      then do
        comp <- liftIO $ GZip.compress . TAR.write <$> (TAR.pack p =<< listDirectory p)
        return (comp, "application/gzip")
      else do
        d <- liftIO $ BL.readFile p
        ct <- liftIO $ CT.deduce p
        return (d, ct)

  let h = hash $ BS.toStrict dat
  destPath <- ST.expandPath (unpack h)

  -- TODO: move to logger
  liftIO $ putStrLn $ "content type: " ++ CT.unpack ct
  liftIO $ putStrLn $ "path: " ++ destPath
  (File t h ct <$) <$> ST.writeFile (unpack h) dat

newEntry ::
  ( MonadIO m,
    HasTitle a,
    PersistRecordBackend a backend,
    PersistRecordBackend VCRoot backend,
    PersistStoreWrite backend
  ) =>
  (UTCTime -> VCRootId -> a) ->
  ReaderT backend m (Key a)
newEntry f = do
  time <- liftIO getCurrentTime
  root <- insert VCRoot
  insert $ f time root

editEntry ::
  ( MonadIO m,
    HasTitle a,
    PersistRecordBackend a backend,
    PersistRecordBackend VCRoot backend,
    PersistStoreWrite backend
  ) =>
  VCRootId ->
  (UTCTime -> VCRootId -> a) ->
  ReaderT backend m (Key a)
editEntry r f = do
  time <- liftIO getCurrentTime
  insert $ f time r

allVerseVs :: (MonadIO m) => Verse -> ReaderT SqlBackend m [Entity Verse]
allVerseVs v = selectList [VerseRoot ==. verseRoot v] [Desc VerseTime]

allPageVs :: (MonadIO m) => Page -> ReaderT SqlBackend m [Entity Page]
allPageVs p = selectList [PageRoot ==. pageRoot p] [Desc PageTime]

allFileVs :: (MonadIO m) => File -> ReaderT SqlBackend m [Entity File]
allFileVs f = selectList [FileRoot ==. fileRoot f] [Desc FileTime]
