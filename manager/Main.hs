module Main where

import Control.Monad (join)
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans
import Control.Monad.Trans.Resource (MonadUnliftIO, runResourceT)
import Data.ByteString qualified as BS
import Data.Entities as ES
import Data.Text (Text, pack)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime)
import Database.Persist.Sqlite
import System.Directory (doesPathExist)
import System.Environment (getArgs)
import System.Storage
import System.Storage.Native
import Text.Read (readMaybe)
import UnliftIO.Resource (ResourceT)
import Output
import Messages

main :: IO ()
main = getArgs >>= parseArgs

defaultRoot :: FilePath
defaultRoot = "/tmp/bookshelf"

parseArgs :: [String] -> IO ()
parseArgs ("--db" : db : args) = runSql db $ parseArgs_ args
parseArgs args = runSql "test.db3" $ parseArgs_ args

runSql :: MonadUnliftIO m => String -> ReaderT SqlBackend (ResourceT (LoggingT m)) a -> m a
runSql db f =
  runStderrLoggingT $
    filterLogger (\_ l -> l >= LevelError) $
      withSqlitePool (pack db) 10 $
        \pool ->
          runResourceT $
            flip runSqlPool pool $
              runMigration migrateAll >> f

parseArgs_ :: (MonadUnliftIO m) => [String] -> ReaderT SqlBackend (ResourceT m) ()
parseArgs_ ("--db" : _ : _) = liftIO $ putStrLn "Duplicate db specfication"
parseArgs_ ("verse" : t : content : file) = do
  liftIO $ putStrLn "adding verse..."
  let t' = pack t
  updateF <- getUpdateFunc VerseTitle t'
  liftIO . print
    =<< updateF . Verse t' (pack content)
    =<< parseFile file
  where
    parseFile [f] = do
      isFile <- liftIO $ doesPathExist f
      let fileKeyM = toSqlKey <$> readMaybe f
      fileEntM <- join <$> mapM get fileKeyM
      case (isFile, fileKeyM, fileEntM) of
        (True, _, _) -> Just <$> upload defaultRoot t f
        (_, Just fid, Just fent) ->
          liftIO (putStrLn (linkingWith fent))
            >> return (Just fid)
        (_, Just fid, Nothing) -> error $ fileIdNotFound $ fromSqlKey fid
        _ -> error $ cantDeduceFile f
    parseFile [] = return Nothing
    parseFile (_ : extra) =
      error $ "unexpected arguments: \"" ++ unwords extra ++ "\""
parseArgs_ ["hash", path] =
  liftIO $ BS.readFile path >>= TIO.putStrLn . hash
parseArgs_ ["update", "verse", v] = do
  liftIO $ putStrLn $ "updating " ++ show v ++ "..."
  let verseKey :: Key Verse = toSqlKey $ read v
  v' <- get verseKey >>= maybe (error "Verse not found") return
  verseChainUpdate v'
    >>= maybe (liftIO $ putStrLn "Up to date") (>>= liftIO . print)
parseArgs_ ["upload", t, path] =
  upload defaultRoot t path >>= liftIO . print
parseArgs_ ("list" : "verses" : _) = outputAll ([] :: [Verse])
parseArgs_ ("list" : "files" : _) = outputAll ([] :: [File])
parseArgs_ ("list" : "pages" : _) = outputAll ([] :: [Page])
parseArgs_ _ = liftIO $ do
  putStrLn "usage: manager [--db <db>]"
  putStrLn "\tverse ..."
  putStrLn "\tupdate <verse> ..."
  putStrLn "\tlist <verse|file|page> ..."
  putStrLn "\tupload ..."
  putStrLn "\thash ..."

-- | Search for an entry with a similar title.
-- If found, returns curried editEntry, or newEntry otherwise.
getUpdateFunc ::
  ( HasTimestamp a,
    HasRoot a,
    HasTitle a,
    MonadIO m,
    PersistEntity a,
    PersistEntityBackend a ~ SqlBackend
  ) =>
  EntityField a Text ->
  Text ->
  ReaderT SqlBackend m ((UTCTime -> VCRootId -> a) -> ReaderT SqlBackend m (Key a))
getUpdateFunc titleF t = do
  parent <- selectFirst [titleF ==. t] []
  case parent of
    Nothing -> return newEntry
    Just p -> do
      let p' = entityVal p
      liftIO $ putStrLn $ "Previous version: " ++ show (time p')
      return $ editEntry $ versionRoot p'

upload :: (MonadIO m) => FilePath -> String -> FilePath -> ReaderT SqlBackend (ResourceT m) (Key File)
upload root t p = do
  liftIO $ putStrLn "uploading..."
  let t' = pack t
  updateF <- getUpdateFunc FileTitle t'
  f <-
    liftIO (runNativeStorage (newFile (pack t) p) root)
      >>= either errorHandler return
  updateF f
  where
    errorHandler NotOverwritng = error "File exists, not overwriting"
    errorHandler e = error $ "Unexpected error: " ++ show e
