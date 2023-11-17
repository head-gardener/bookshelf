module Routers where

import Data.Entities as ES
import Data.Text as T
import Database.Persist.Sqlite
import Foundation
import Render
import System.Storage qualified as ST
import Yesod

getHomeR :: Handler Html
getHomeR = do
  verses <- runDB $ selectList [] [LimitTo 5, Desc VerseId]
  pages <- runDB $ selectList [] [LimitTo 5, Desc PageId]
  files <- runDB $ selectList [] [LimitTo 5, Desc FileId]
  defaultLayout $ do
    setTitle "BookShelf"
    [whamlet|<h1>Latest Verses|]
    mapM_ reference verses
    [whamlet|<h1>Latest Pages|]
    mapM_ reference pages
    [whamlet|<h1>Latest Files|]
    mapM_ reference files

defaultPage ::
  ( PersistEntityBackend a ~ SqlBackend,
    PersistEntity a,
    HasVersions a,
    Drawable a
  ) =>
  Key a ->
  HandlerFor BookShelf Html
defaultPage objId = do
  obj <- runDB (get404 objId)
  versions <- runDB $ allVersions obj
  defaultLayout $ do
    draw Timestamped obj
    [whamlet|<h3>History:|]
    mapM_ reference versions

getVerseR :: VerseId -> Handler Html
getVerseR = defaultPage

getPageR :: PageId -> Handler Html
getPageR = defaultPage

getFileR :: FileId -> Handler Html
getFileR = defaultPage

getStorageR :: Text -> Handler Html
getStorageR fileName =
  runDB (getBy404 $ UniqueFName fileName) >>= sendF . entityVal
  where
    sendF f = do
      path <-
        either (const notFound) return
          =<< runStorage (ST.exportPath $ T.unpack $ ES.fileName f)
      sendFile (fileType f) path
