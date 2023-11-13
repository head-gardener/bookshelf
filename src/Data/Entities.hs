module Data.Entities where

import Control.Monad.Reader
import Data.Text qualified as T
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import Data.Text (unpack)
import Data.ContentType

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Verse
  title T.Text
  content T.Text
  file (Maybe FileId)
  time UTCTime
Page
  title T.Text
  verses [VerseId]
  time UTCTime
File
  title T.Text
  name T.Text
  type ContentType
  time UTCTime
  UniqueFName name
VerceVC
  title T.Text
  children [VerseId]
  summary [T.Text]
|]

filePath :: File -> FilePath
filePath = ("/tmp/bookshelf/" ++) . unpack . fileName

populate :: (MonadIO m) => ReaderT SqlBackend m ()
populate = do
  v1 <- insert testV
  v2 <- insert testV
  f1 <- insert testF
  _ <- insert testI
  v3 <- insert $ Verse "Verse with a file" "Test" (Just f1) time
  insert_ $ Page "Sample Page" [v1, v2, v3] time
  where
    testV :: Verse
    testV = Verse "hey" "this is a test verse" Nothing time

    testI :: File
    testI = File "Test Image!!" "testimage" "image/png" time
    testF :: File
    testF = File "Test File!!" "testfile" "plain/text" time

    time :: UTCTime
    time = UTCTime (fromGregorian 1970 1 1) 0
