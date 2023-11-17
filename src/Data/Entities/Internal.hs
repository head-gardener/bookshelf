module Data.Entities.Internal where

import Data.Time
import Data.Text
import Data.ContentType
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
