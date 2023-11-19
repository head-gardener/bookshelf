module Messages where

import Data.Entities
import Data.Int (Int64)
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

usage :: IO ()
usage =
  mapM_
    putStrLn
    [ "Usage: manager [--db <db>] COMMAND",
      "",
      "A command line tool for managing bookshelf database.",
      "",
      "Commands:",
      "\tpage <title> <verse-ids>",
      "\tverse <title> <content> [file-path|file-id]",
      "\tupdate {verse page} <id>",
      "\tlist {verse file page} <filters>",
      "\tupload <file-path>",
      "\thash <file-path>"
    ]
