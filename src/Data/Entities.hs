module Data.Entities where

import Control.Monad.Reader
import Data.ContentType (ContentType ())
import Data.Maybe (fromJust)
import Data.Text (Text (), unpack)
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH

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

filePath :: File -> FilePath
filePath = defaultPath . unpack . fileName

defaultPath :: String -> FilePath
defaultPath = ("/tmp/bookshelf/" ++)

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
