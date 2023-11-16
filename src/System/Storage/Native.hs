{-# OPTIONS_GHC -Wno-orphans #-}

module System.Storage.Native where

import Control.Monad.Reader
import Data.ByteString.Lazy qualified as BL
import System.Directory
import System.FilePath ((</>))
import System.Storage

type Native = ReaderT FilePath IO

instance StorageMonad Native where
  writeFile f s = do
    path <- expandPath f
    destExists <- liftIO $ doesPathExist path
    when destExists $ error "File already stored, not overwriting"
    liftIO $ BL.writeFile path s

  readFile = expandPath >=> liftIO . BL.readFile
  expandPath p = do
    r <- ask
    return $ p </> r

instance StorageExport Native where
  exportPath = expandPath

runNativeStorage :: Native a -> FilePath -> IO a
runNativeStorage n r = runReaderT n' r
  where
    n' = do
      rootExists <- liftIO $ doesDirectoryExist r
      unless rootExists $ liftIO $ error $ "Root " ++ r ++ " doesn't exist."
      n
