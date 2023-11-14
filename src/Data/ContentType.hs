module Data.ContentType where

import Data.ByteString.UTF8
import Magic

type ContentType = ByteString

pack :: String -> ContentType
pack = fromString

unpack :: ContentType -> String
unpack = toString

deduce :: FilePath -> IO ContentType
deduce path = do
  magic <- magicOpen [MagicMimeType]
  magicLoadDefault magic
  pack <$> magicFile magic path
