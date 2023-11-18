module Messages where

import Data.Int (Int64)
import Data.Entities
import Data.Text (unpack)

fileIdNotFound :: Int64 -> String
fileIdNotFound fid = "File #" ++ show fid ++ " not found"

linkingWith :: File -> String
linkingWith f =
  "Linking with \""
    ++ unpack (fileTitle f)
    ++ "\", "
    ++ show (fileTime f)

cantDeduceFile :: String -> String
cantDeduceFile f =
  "Couldn't deduce file from \""
    ++ f
    ++ "\" - not a path, nor a file id"
