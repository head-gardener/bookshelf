module Main (main) where

import Control.Monad (forM, join)
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
import Messages
import Output
import System.Directory (doesPathExist)
import System.Environment (getArgs)
import System.Storage
import System.Storage.Native
import Text.Read (readMaybe)
import UnliftIO.Resource (ResourceT)

main :: IO ()
main = getArgs >>= parseArgs

defaultRoot :: FilePath
defaultRoot = "/tmp/bookshelf"

parseArgs :: [String] -> IO ()
parseArgs ("--help": _) = usage
parseArgs ("-h": _) = usage
parseArgs ("--db" : db : args) = runSql db $ parseArgs_ args
parseArgs args = runSql "test.db3" $ parseArgs_ args

runSql :: MonadUnliftIO m => String -> ReaderT SqlBackend (ResourceT (LoggingT m)) a -> m a
runSql db f = runStderrLoggingT $
  filterLogger (\_ l -> l >= LevelError) $
    withSqlitePool (pack db) 10 $ \pool ->
      runResourceT (flip runSqlPool pool $ runMigration migrateAll >> f)

parseArgs_ :: (MonadUnliftIO m) => [String] -> ReaderT SqlBackend (ResourceT m) ()
parseArgs_ ("--db" : _ : _) = liftIO $ putStrLn "Unexpected --db argument"
parseArgs_ ("page" : t : vs) = do
  let vsM :: [Maybe (Key Verse)] = fmap ((toSqlKey <$>) . readMaybe) vs
  vs' <- forM (zip vsM vs) $
    \(v, s) -> maybe (error $ s ++ " should be a verse id") return v
  maybe (error "A verse key not on db!") (const $ return ()) . sequence
    =<< forM vs' get
  f <- getUpdateFunc PageTitle $ pack t
  liftIO . print =<< f (Page (pack t) vs')
parseArgs_ ("verse" : t : content : file) = do
  let t' = pack t
  updateF <- getUpdateFunc VerseTitle t'
  liftIO . print
    =<< updateF . Verse t' (pack content)
    =<< parseFile t file
parseArgs_ ["hash", path] =
  liftIO $ BS.readFile path >>= TIO.putStrLn . hash
parseArgs_ ["update", "verse", v] = do
  let verseKey = toSqlKey $ read v
  v' <- get verseKey >>= maybe (error "Verse not found") return
  verseChainUpdate v'
    >>= maybe (liftIO $ putStrLn "Up to date") (>>= liftIO . print)
parseArgs_ ["update", "page", p] = do
  let pageKey = toSqlKey $ read p
  p' <- get pageKey >>= maybe (error "Page not found") return
  pageChainUpdate p'
    >>= maybe (liftIO $ putStrLn "Up to date") (>>= liftIO . print)
parseArgs_ ["upload", t, path] =
  upload defaultRoot t path >>= liftIO . print
parseArgs_ ("list" : "verses" : fs) =
  outputAll =<< liftIO (parseFilters fs :: IO [Filter Verse])
parseArgs_ ("list" : "pages" : fs) =
  outputAll =<< liftIO (parseFilters fs :: IO [Filter Page])
parseArgs_ ("list" : "files" : fs) =
  outputAll =<< liftIO (parseFilters fs :: IO [Filter File])
parseArgs_ _ = liftIO usage

-- is this lazy?
parseFilters :: DBEntity a => [String] -> IO [Filter a]
parseFilters ("title" : t : fs) = ((titleField ==. pack t) :) <$> parseFilters fs
parseFilters ("root" : r : fs) = ((rootField ==. toSqlKey (read r)) :) <$> parseFilters fs
parseFilters (f : _) = error $ "Invalid filter: " ++ f
parseFilters [] = return []

parseFile ::
  MonadIO m =>
  String ->
  [FilePath] ->
  ReaderT SqlBackend m (Maybe (Key File))
parseFile t [f] = do
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
parseFile _ [] = return Nothing
parseFile _ (_ : extra) =
  error $ "unexpected arguments: \"" ++ unwords extra ++ "\""

-- | Search for an entry with a similar title.
-- If found, returns curried editEntry, or newEntry otherwise.
getUpdateFunc ::
  ( DBEntity a,
    MonadIO m,
    SafeToInsert a,
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

upload ::
  (MonadIO m) =>
  FilePath ->
  String ->
  FilePath ->
  ReaderT SqlBackend m (Key File)
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
