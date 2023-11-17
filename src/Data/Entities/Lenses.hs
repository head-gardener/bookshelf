module Data.Entities.Lenses where

import Control.Monad.Reader
import Data.Entities.Internal
import Data.Text
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql

-- Timestamp
class (PersistEntity a) => HasTimestamp a where
  time :: a -> UTCTime
  timeField :: EntityField a UTCTime

instance HasTimestamp Verse where
  time = verseTime
  timeField = VerseTime

instance HasTimestamp File where
  time = fileTime
  timeField = FileTime

instance HasTimestamp Page where
  time = pageTime
  timeField = PageTime

-- Root
class (PersistEntity a) => HasRoot a where
  versionRoot :: a -> VCRootId
  rootField :: EntityField a VCRootId

instance HasRoot Verse where
  versionRoot = verseRoot
  rootField = VerseRoot

instance HasRoot File where
  versionRoot = fileRoot
  rootField = FileRoot

instance HasRoot Page where
  versionRoot = pageRoot
  rootField = PageRoot

-- Title
class (PersistEntity a) => HasTitle a where
  title :: a -> Text
  titleField :: EntityField a Text

instance HasTitle Verse where
  title = verseTitle
  titleField = VerseTitle

instance HasTitle File where
  title = fileTitle
  titleField = FileTitle

instance HasTitle Page where 
  title = pageTitle
  titleField = PageTitle

-- Versions
class (HasRoot a, HasTimestamp a, PersistEntityBackend a ~ SqlBackend) => HasVersions a where
  allVersions :: (MonadIO m) => a -> ReaderT SqlBackend m [Entity a]
  allVersions a = selectList [rootField ==. versionRoot a] [Desc timeField]

  lastVersion :: (MonadIO m) => a -> ReaderT SqlBackend m (Maybe (Entity a))
  lastVersion a = selectFirst [rootField ==. versionRoot a] [Desc timeField]

instance HasVersions Verse

instance HasVersions File

instance HasVersions Page
