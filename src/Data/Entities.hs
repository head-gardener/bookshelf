module Data.Entities where

import Control.Monad.Reader
import Data.ContentType
import Data.Text (Text(), unpack)
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import Data.Maybe (fromJust)

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
File
  title Text
  name Text
  type ContentType
  time UTCTime
  UniqueFName name
VCRoot
  title Text
|]

-- class HasRoot a where
--   root :: a -> VCRootId
--   type Root (a) :: EntityField a VCRootId

-- instance HasRoot Verse where
--   root = verseRoot

filePath :: File -> FilePath
filePath = ("/tmp/bookshelf/" ++) . unpack . fileName

newVerse :: MonadIO m => Text -> Text -> Maybe (Key File) -> ReaderT SqlBackend m (Key Verse)
newVerse t c f = do
  time <- liftIO getCurrentTime
  vc <- insert $ VCRoot t
  insert $ Verse t c f time vc

newVerse_ :: MonadIO m => Text -> Text -> Maybe (Key File) -> ReaderT SqlBackend m ()
newVerse_ t c f = void $ newVerse t c f

-- TODO: (Verse -> Verse)
editVerse :: MonadIO m => Verse -> Text -> Text -> Maybe (Key File) -> ReaderT SqlBackend m (Key Verse)
editVerse v t c  f = do
  time <- liftIO getCurrentTime
  insert $ Verse t c Nothing time $ verseRoot v

editVerse_ :: MonadIO m => Text -> Text -> Maybe (Key File) -> ReaderT SqlBackend m ()
editVerse_ t c f = void $ newVerse t c f

allVerseVs :: (MonadIO m) => Verse -> ReaderT SqlBackend m [Entity Verse]
allVerseVs v = selectList [VerseRoot ==. verseRoot v] [Desc VerseTime]

-- allVersions :: (MonadIO m, HasRoot a) => a -> ReaderT SqlBackend m [Entity a]
-- allVersions r = selectList [EntityField a VCRootId ==. root r] []

populate :: MonadIO m => ReaderT SqlBackend m ()
populate = do
  v1 <- newVerse "test verse 1" "this is a test verse" Nothing
  verse1 <- fromJust <$> get v1
  v2 <- editVerse verse1 "test verse 2" "this is a test verse revision" Nothing
  ft <- insert testF
  fi <- insert testI
  v3 <- newVerse "verse with an image file" "this test verse contains a file" (Just fi)
  verse3 <- fromJust <$> get v3
  _ <- editVerse verse3 "verse with a text file" (verseContent verse3) (Just ft)
  insert_ $ Page "Sample Page" [v1, v2, v3] time
  where
    testI :: File
    testI = File "Test Image!!" "testimage" "image/png" time
    testF :: File
    testF = File "Test File!!" "testfile" "plain/text" time

    time :: UTCTime
    time = UTCTime (fromGregorian 1970 1 1) 0
