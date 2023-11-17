module Data.Entities.Lenses where

import Data.Entities.Internal
import Data.Text
import Data.Time (UTCTime)
import Database.Persist

-- Timestamp
class HasTimestamp a where time :: a -> UTCTime

instance HasTimestamp a => HasTimestamp (Entity a) where
  time = time . entityVal

instance HasTimestamp Verse where time = verseTime

instance HasTimestamp File where time = fileTime

instance HasTimestamp Page where time = pageTime

-- Root
class HasRoot a where versionRoot :: a -> VCRootId

instance HasRoot a => HasRoot (Entity a) where
  versionRoot = versionRoot . entityVal

instance HasRoot Verse where versionRoot = verseRoot

instance HasRoot File where versionRoot = fileRoot

instance HasRoot Page where versionRoot = pageRoot

-- Title
class HasTitle a where title :: a -> Text

instance HasTitle a => HasTitle (Entity a) where title = title . entityVal

instance HasTitle Verse where title = verseTitle

instance HasTitle File where title = fileTitle

instance HasTitle Page where title = pageTitle
