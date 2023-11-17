module System.Storage.Error where

data StorageError = NotOverwritng | NotFound
  deriving (Eq, Show)
