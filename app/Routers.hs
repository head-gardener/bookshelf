module Routers where

import Foundation
import Data.Entities
import Render
import Database.Persist.Sqlite
import Yesod
import Data.Text

getHomeR :: Handler Html
getHomeR = do
  verses <- runDB $ selectList [] [Desc VerseId]
  pages <- runDB $ selectList [] [Desc PageId]
  files <- runDB $ selectList [] [Desc FileId]
  defaultLayout $ do
    setTitle "BookShelf"
    [whamlet|<h1>Latest Verses|]
    listVEs verses
    [whamlet|<h1>Latest Pages|]
    mapM_ (summaryP . entityVal) pages
    [whamlet|<h1>Latest Files|]
    mapM_ (drawF . entityVal) files

getVerseR :: VerseId -> Handler Html
getVerseR verseId = runDB (get404 verseId) >>= defaultLayout . drawV

getPageR :: PageId -> Handler Html
getPageR pageId = runDB (get404 pageId) >>= defaultLayout . drawP

getFileR :: FileId -> Handler Html
getFileR fileId = runDB (get404 fileId) >>= defaultLayout . showFile

getStorageR :: Text -> Handler Html
getStorageR fileName = 
  runDB (getBy404 $ UniqueFName fileName) >>= sendF . entityVal
  where sendF f = sendFile (fileType f) . filePath $ f
