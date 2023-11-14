module Render where

import Data.ContentType as CT
import Data.Entities as ES
import Data.Text qualified as T
import Data.Time
import Foundation
import Yesod

data Format = Timestamped | Minimal
  deriving (Show, Eq)

class (HasTitle a) => Drawable a where
  timestamp :: a -> UTCTime
  route :: Entity a -> Route BookShelf
  drawContent :: a -> Widget
  drawSummary :: a -> Widget
  drawSummary = drawContent

-- Is this a good idea?
instance Drawable a => Drawable (Entity a) where
  timestamp = timestamp . entityVal
  route = route
  drawContent = drawContent . entityVal
  drawSummary = drawSummary . entityVal

draw :: Drawable a => Format -> a -> Widget
draw f a =
  [whamlet|
    <h3>#{title a}
    $if f == Timestamped
      <p>#{show $ timestamp a}
    ^{drawContent a}
  |]

summary :: Drawable a => a -> Widget
summary a =
  [whamlet|
    <h3>#{title a}
    ^{drawSummary a}
  |]

reference :: Drawable a => Entity a -> Widget
reference a =
  [whamlet|
    <h3>
      <a href=@{route a}>#{title a}
    ^{drawSummary a}
  |]

instance Drawable Verse where
  timestamp = verseTime
  route = VerseR . entityKey
  drawContent v = do
    -- TODO: this sucks
    file <- case verseFile v of
      Just f -> liftHandler $ runDB $ get f
      Nothing -> return Nothing
    [whamlet|
      <p>#{verseContent v}
      ^{mapM_ drawContent file}
    |]
  drawSummary v =
    [whamlet|
    <p>#{T.append (T.take 25 $ verseContent v) "..."}
  |]

instance Drawable Page where
  timestamp = pageTime
  route = PageR . entityKey
  drawContent p = do
    verses <- liftHandler $ mapM (runDB . get404) $ pageVerses p
    [whamlet|^{mapM_ (draw Minimal) verses}|]
  drawSummary p = do
    verses <- liftHandler $ mapM (runDB . get404) $ pageVerses p
    [whamlet|
      $forall v <- verses
        <p>#{title v}
    |]

instance Drawable File where
  timestamp = fileTime
  route = FileR . entityKey
  drawContent f = do
    case fileType f of
      -- TODO: safe load file
      "text/plain" -> do
        c <- liftIO (readFile $ filePath f)
        toWidget
          [hamlet|
          <p>Preview:
          <p>#{take 100 c}
        |]
      "image/png" -> do
        toWidget
          [hamlet|
          <img
            src=@{StorageR $ ES.fileName f}
            alt=#{fileTitle f}
            width=300 height=300>
        |]
      _ -> toWidget [hamlet|<p>can't preview|]
    toWidget [hamlet|<p><a href=@{StorageR $ ES.fileName f}>Download|]
  drawSummary f = toWidget [hamlet|<p>#{CT.unpack $ fileType f}|]
