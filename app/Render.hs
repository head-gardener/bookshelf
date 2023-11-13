module Render where

import Data.Entities as ES
import Data.Text qualified as T
import Foundation
import Yesod

drawV :: Verse -> Widget
drawV v = do
  [whamlet|
    <h3>#{verseTitle v}
    <p>#{verseContent v}
    <p>#{show $ verseTime v}
  |]

-- \$maybe f <- verseFile v
--   ^{showFile f}
-- \$nothing

drawP :: Page -> Widget
drawP p =
  toWidget
    [whamlet|
      <h3>#{ES.pageTitle p}
      <p>#{show $ pageTime p}
    |]

-- \^{mapM_ drawV verses}

drawF :: File -> Widget
drawF f@(File title _ _ time) =
  toWidget
    [whamlet|
      <h3>#{title}
      <p>#{show time}
      ^{showFile f}
    |]

-- TODO: safe load file
showFile :: File -> Widget
showFile f = case fileType f of
  "plain/text" -> do
    liftIO $ print $ filePath f
    c <- liftIO (readFile $ filePath f)
    toWidget [hamlet|<p>#{c}|]
  "image/png" -> do
    toWidget
      [hamlet|
      <img
        src=@{StorageR $ ES.fileName f}
        alt=#{fileTitle f}
        width=300 height=300>
    |]
  _ -> toWidget [hamlet|<p>can't preview|]

summaryV_ :: Maybe VerseId -> Verse -> Widget
summaryV_ vid ver =
  toWidget
    [hamlet|
    ^{title vid ver}
    <p>#{contentSummary ver}
  |]
  where
    title (Just i) v =
      [hamlet|<h3>
        <a href=@{VerseR i}>#{verseTitle v}|]
    title _ v = [hamlet|<h3>#{verseTitle v}|]

    contentSummary v = T.append (T.take 25 $ verseContent v) "..."

summaryEV :: Entity Verse -> Widget
summaryEV (Entity vid ver) = summaryV_ (Just vid) ver

summaryV :: Verse -> Widget
summaryV = summaryV_ Nothing

listVE_ :: (a -> Widget) -> [a] -> Widget
listVE_ f es =
  [whamlet|
    ^{mapM_ f es}
  |]

listVEs :: [Entity Verse] -> Widget
listVEs = listVE_ summaryEV

listVs :: [Verse] -> Widget
listVs = listVE_ summaryV

summaryP :: Page -> Widget
summaryP p =
  [whamlet|
    <h2>#{ES.pageTitle p}
  |]

-- \^{listVs $ ES.pageVerses p}
