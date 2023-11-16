module Data.Entities where

import Codec.Archive.Tar qualified as TAR
import Codec.Compression.GZip qualified as GZip
import Control.Monad.Reader
import Data.Binary qualified as BN
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.ContentType (ContentType ())
import Data.ContentType qualified as CT
import Data.Digest.CityHash
import Data.Maybe (fromJust)
import Data.Text (Text (), unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import System.Directory
import System.FilePath ((</>))

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
  format . TE.decodeUtf8 . B64.encode . BS.toStrict . BN.encode . cityHash128
  where
    format = trim . T.replace "/" "-"
    trim s = T.take (T.length s - 2) s

filePath :: File -> FilePath
filePath = defaultPath . unpack . fileName

newFile :: Text -> FilePath -> IO (UTCTime -> VCRootId -> File)
newFile t p = do
  isDir <- doesDirectoryExist p
  (path, ct) <-
    if isDir
      then do
        let temp = defaultPath "compression"
        BL.writeFile temp . GZip.compress . TAR.write
          =<< TAR.pack p
          =<< listDirectory p
        return (temp, "application/gzip")
      else (p,) <$> CT.deduce p

  n <- hash <$> BS.readFile path
  let destPath = defaultPath (unpack n)

  -- TODO: move to logger
  putStrLn $ "content type: " ++ CT.unpack ct
  putStrLn $ "path: " ++ destPath

  destExists <- doesFileExist destPath
  if destExists
    then error "File already stored, not overwriting"
    else copyFile path $ defaultPath (unpack n)
  return $ File t n ct

defaultPath :: String -> FilePath
defaultPath = ("/tmp/bookshelf/" </>)

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

populate :: MonadIO m => ReaderT SqlBackend m ()
populate = do
  v1 <- newEntry $ Verse "test verse 1" "this is a test verse" Nothing
  verse1 <- fromJust <$> get v1
  v2 <- editEntry (verseRoot verse1) $ Verse "test verse 2" "this is a test verse revision" Nothing
  ft <- newEntry testF
  file1 <- fromJust <$> get ft
  fi <- editEntry (fileRoot file1) testI
  v3 <- newEntry $ Verse "verse with an image file" "this test verse contains a file" (Just fi)
  verse3 <- fromJust <$> get v3
  _ <- editEntry (verseRoot verse3) $ Verse "verse with a text file" (verseContent verse3) (Just ft)
  _ <- newEntry $ Page "Sample Page" [v1, v2, v3]
  return ()
  where
    testI = File "Test Image!!" "testimage" "image/png"
    testF = File "Test File!!" "testfile" "plain/text"
