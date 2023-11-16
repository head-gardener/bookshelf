module System.Storage (StorageMonad (..), StorageExport (..)) where

import Data.ByteString.Lazy qualified as BL
import Control.Monad.IO.Class (MonadIO)

class (MonadIO m) => StorageMonad m where
  readFile :: FilePath -> m BL.ByteString
  writeFile :: FilePath -> BL.ByteString -> m ()
  expandPath :: FilePath -> m FilePath

class (MonadIO m) => StorageExport m where
  exportPath :: FilePath -> m FilePath
