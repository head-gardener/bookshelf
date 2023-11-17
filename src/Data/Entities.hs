module Data.Entities
  ( newEntry,
    editEntry,
    allVerseVs,
    allPageVs,
    allFileVs,
    module Data.Entities.Internal,
    module Data.Entities.Lenses,
    module Data.Entities.Storage,
  )
where

import Control.Monad.Reader
import Data.Entities.Internal
import Data.Entities.Lenses
import Data.Entities.Storage
import Data.Time
import Database.Persist.Sql

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
