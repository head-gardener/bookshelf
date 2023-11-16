module Routers where

import Data.Entities as ES
import Data.Text as T
import Database.Persist.Sqlite
import Foundation
import Render
import Yesod
import System.Storage qualified as ST

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
getPageR pageId = do
  page <- runDB (get404 pageId)
  versions <- runDB $ allPageVs page
  defaultLayout $ do
    draw Timestamped page
    [whamlet|<h3>History:|]
    mapM_ reference versions

getFileR :: FileId -> Handler Html
getFileR fileId = do
  file <- runDB (get404 fileId)
  versions <- runDB $ allFileVs file
  defaultLayout $ do
    draw Timestamped file
    [whamlet|<h3>History:|]
    mapM_ reference versions

getStorageR :: Text -> Handler Html
getStorageR fileName =
  runDB (getBy404 $ UniqueFName fileName) >>= sendF . entityVal
  where
    sendF f = do
      path <- runStorage $ ST.exportPath $ T.unpack $ ES.fileName f
      sendFile (fileType f) path
