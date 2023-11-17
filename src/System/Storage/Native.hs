{-# OPTIONS_GHC -Wno-orphans #-}

module System.Storage.Native where

import Control.Exception
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
    if destExists
      then return $ Left NotOverwritng
      else do
        liftIO $ BL.writeFile path s
        return $ Right ()

  readFile =
    expandPath
      >=> liftIO . tryJust (\(_ :: IOException) -> Just NotFound) . BL.readFile

  expandPath p = do
    r <- ask
    return $ r </> p

instance StorageExport Native where
  exportPath p = do
    p' <- expandPath p
    destExists <- liftIO $ doesPathExist p'
    return $
      if not destExists
        then Left NotFound
        else Right p'

runNativeStorage :: Native a -> FilePath -> IO a
runNativeStorage n r = runReaderT n' r
  where
    n' = do
      rootExists <- liftIO $ doesDirectoryExist r
      unless rootExists $ liftIO $ error $ "Root " ++ r ++ " doesn't exist."
      n
