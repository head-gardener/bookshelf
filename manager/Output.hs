module Output where

import Control.Monad.Reader
import Data.Entities
import Database.Persist.Sql
import Data.ContentType qualified as CT
import Data.Text (unpack)
import Data.List (intercalate)

class (DBEntity a, ToBackendKey SqlBackend a) => Output a where
  output :: Entity a -> IO ()
  output a = do
    let a' = entityVal a
    putStrLn $
      intercalate
        " - "
        [unpack $ title a', show $ fromSqlKey $ entityKey a, show $ time a']
    putStrLn $ "root: " ++ show (fromSqlKey $ versionRoot a')
    outputContent a'

  outputAll :: (MonadIO m, Output a) => [a] -> ReaderT SqlBackend m ()
  outputAll _ = do
    vs :: [Entity a] <- selectList [] [Desc timeField]
    liftIO $ mapM_ ((>> putStrLn "") . output) vs

  outputContent :: a -> IO ()

instance Output Verse where
  outputContent v = do
    mapM_ (putStrLn . ("attachment: " ++) . show . fromSqlKey) $ verseFile v
    mapM_ putStrLn $ take 5 $ lines $ unpack $ verseContent v

instance Output File where
  outputContent f = do
    putStrLn $ CT.unpack (fileType f) ++ ", " ++ unpack (fileName f)

instance Output Page where
  outputContent p = do
    mapM_ (putStrLn . ("verse " ++) . show . fromSqlKey) $ pageVerses p
