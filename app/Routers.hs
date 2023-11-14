module Routers where

import Foundation
import Data.Entities
import Render
import Database.Persist.Sqlite
import Yesod
import Data.Text

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

getVerseR :: VerseId -> Handler Html
getVerseR verseId = do
  verse <- runDB (get404 verseId)
  versions <- runDB $ allVerseVs verse
  defaultLayout $ do
    draw Timestamped verse
    [whamlet|<h3>History:|]
    mapM_ reference versions

getPageR :: PageId -> Handler Html
getPageR pageId = runDB (get404 pageId) >>= defaultLayout . draw Timestamped

getFileR :: FileId -> Handler Html
getFileR fileId = runDB (get404 fileId) >>= defaultLayout . draw Timestamped

getStorageR :: Text -> Handler Html
getStorageR fileName = 
  runDB (getBy404 $ UniqueFName fileName) >>= sendF . entityVal
  where sendF f = sendFile (fileType f) . filePath $ f
