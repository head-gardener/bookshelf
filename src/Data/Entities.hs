module Data.Entities
  ( newEntry,
    editEntry,
    module Data.Entities.Internal,
    module Data.Entities.Lenses,
    module Data.Entities.Storage,
    verseChainUpdate,
    pageChainUpdate,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Entities.Internal
import Data.Entities.Lenses
import Data.Entities.Storage
import Data.Time
import Database.Persist.Sql

newEntry ::
  ( MonadIO m,
    HasTitle a,
    SafeToInsert a,
    PersistRecordBackend a backend,
    PersistRecordBackend VCRoot backend,
    PersistStoreWrite backend
  ) =>
  (UTCTime -> VCRootId -> a) ->
  ReaderT backend m (Key a)
newEntry f = do
  now <- liftIO getCurrentTime
  root <- insert VCRoot
  insert $ f now root

editEntry ::
  ( MonadIO m,
    HasTitle a,
    SafeToInsert a,
    PersistRecordBackend a backend,
    PersistRecordBackend VCRoot backend,
    PersistStoreWrite backend
  ) =>
  VCRootId ->
  (UTCTime -> VCRootId -> a) ->
  ReaderT backend m (Key a)
editEntry r f = do
  now <- liftIO getCurrentTime
  insert $ f now r

verseChainUpdate ::
  (MonadIO m) =>
  Verse ->
  ReaderT SqlBackend m (Maybe (ReaderT SqlBackend m (Key Verse)))
verseChainUpdate v = runMaybeT $ do
  Just fileId <- return $ verseFile v
  file <- MaybeT $ get fileId
  lastV <- fmap entityKey $ MaybeT $ lastVersion file
  guard $ lastV /= fileId
  return $ editEntry (versionRoot v) $ Verse (title v) (verseContent v) (Just lastV)

pageChainUpdate ::
  (MonadIO m) =>
  Page ->
  ReaderT SqlBackend m (Maybe (ReaderT SqlBackend m (Key Page)))
pageChainUpdate p = runMaybeT $ do
  let vs = pageVerses p
  verses <- MaybeT $ sequence <$> mapM get vs
  updated <- fmap entityKey <$> MaybeT (sequence <$> mapM lastVersion verses)
  guard $ not $ null verses
  guard $ vs /= updated
  return $ editEntry (versionRoot p) $ Page (title p) updated
