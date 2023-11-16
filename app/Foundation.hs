module Foundation where

import Data.Entities
import Data.Text
import Database.Persist.Sqlite
import Yesod
import System.Storage.Native

data BookShelf = BookShelf
  { pool :: ConnectionPool,
    root :: FilePath
  }

mkYesodData "BookShelf" $(parseRoutesFile "app/routes")

instance YesodPersist BookShelf where
  type YesodPersistBackend BookShelf = SqlBackend

  runDB action = runSqlPool action =<< getsYesod pool

instance Yesod BookShelf

runStorage :: (MonadHandler m, HandlerSite m ~ BookShelf) => Native a -> m a
runStorage action = liftIO . runNativeStorage action =<< getsYesod root
