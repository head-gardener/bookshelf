module Foundation where

import Database.Persist.Sqlite
import Yesod
import Data.Entities
import Data.Text

newtype BookShelf = BookShelf ConnectionPool

mkYesodData "BookShelf" $(parseRoutesFile "app/routes")

instance YesodPersist BookShelf where
  type YesodPersistBackend BookShelf = SqlBackend

  runDB action = do
    BookShelf pool <- getYesod
    runSqlPool action pool

instance Yesod BookShelf
