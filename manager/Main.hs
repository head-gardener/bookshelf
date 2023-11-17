module Main where

import Control.Monad (join)
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource (runResourceT, MonadUnliftIO)
import Data.ByteString qualified as BS
import Data.Entities as ES
import Data.Int (Int64)
import Data.Text (pack, unpack)
import Data.Text.IO qualified as TIO
import Database.Persist.Sqlite
import System.Directory (doesPathExist)
import System.Environment (getArgs)
import System.Storage
import System.Storage.Native
import Text.Read (readMaybe)
import Data.Pool (Pool)
import UnliftIO.Resource (ResourceT)
import Control.Monad.Reader (ReaderT)

main :: IO ()
main = getArgs >>= parseArgs

defaultRoot :: FilePath
defaultRoot = "/tmp/bookshelf"

parseArgs :: [String] -> IO ()
parseArgs ("--db" : db : rs) = runStderrLoggingT $
  withSqlitePool (pack db) 10 $
    \pool -> liftIO $ parseArgs_ (Just pool) rs
parseArgs args = runStderrLoggingT $
  withSqlitePool "test.db3" 10 $
    \pool -> liftIO $ parseArgs_ (Just pool) args

parseArgs_ :: Maybe (Pool SqlBackend) -> [String] -> IO ()
parseArgs_ _ ("--db" : _ : _) = putStrLn "Duplicate db specfication"
parseArgs_ (Just pool) ("verse" : t : content : file) = do
  putStrLn "adding verse..."
  print
    =<< runSql pool . newEntry . Verse (pack t) (pack content)
    =<< parseFile file
  where
    parseFile :: [String] -> IO (Maybe FileId)
    parseFile [f] = do
      isFile <- doesPathExist f
      let fileKeyM = toSqlKey <$> readMaybe f
      fileEntM <- join <$> mapM (runSql pool . get) fileKeyM
      case (isFile, fileKeyM, fileEntM) of
        (True, _, _) -> Just <$> upload defaultRoot pool t f
        (_, Just fid, Just fent) -> putStrLn (linkingWith fent) >> return (Just fid)
        (_, Just fid, Nothing) -> error $ fileIdNotFound $ fromSqlKey fid
        _ -> error $ cantDeduceFile f
    parseFile [] = return Nothing
    parseFile (_ : extra) =
      error $ "unexpected arguments: \"" ++ unwords extra ++ "\""
parseArgs_ _ ["hash", path] = do
  BS.readFile path >>= TIO.putStrLn . hash
parseArgs_ (Just pool) ["upload", t, path] = upload defaultRoot pool t path >>= print
parseArgs_ _ _ = do
  putStrLn "usage: manager [--db <db>]"
  putStrLn "\tverse ..."
  putStrLn "\tupload ..."
  putStrLn "\thash ..."

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

upload :: FilePath -> Pool SqlBackend -> String -> FilePath -> IO (Key File)
upload root pool t p = do
  putStrLn "uploading..."
  runNativeStorage (newFile (pack t) p) root
    >>= either errorHandler return
    >>= runSql pool . newEntry
  where
    errorHandler NotOverwritng = error "File exists, not overwriting"
    errorHandler e = error $ "Unexpected error: " ++ show e

runSql :: MonadUnliftIO m => Pool SqlBackend -> ReaderT SqlBackend (ResourceT m) a -> m a
runSql pool f =
  runResourceT $
    flip runSqlPool pool $
      runMigration migrateAll >> f
