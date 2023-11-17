module System.Storage
  ( StorageMonad (..),
    StorageExport (..),
    module System.Storage.Error,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy qualified as BL
import System.Storage.Error

class (MonadIO m) => StorageMonad m where
  readFile :: FilePath -> m (Either StorageError BL.ByteString)
  writeFile :: FilePath -> BL.ByteString -> m (Either StorageError ())
  expandPath :: FilePath -> m FilePath

class (StorageMonad m) => StorageExport m where
  exportPath :: FilePath -> m (Either StorageError FilePath)
