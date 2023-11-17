module System.Storage.Error where

data StorageError = NotOverwritng | NotFound | InvalidRoot
  deriving (Eq, Show)
