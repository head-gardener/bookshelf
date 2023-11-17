{-# OPTIONS_GHC -Wno-orphans #-}

module System.Storage.Memory where

import Control.Exception
import Control.Monad.State
import Data.ByteString.Lazy qualified as BL
import Data.Map as Map
import System.Storage

type Memory = StateT (Map FilePath BL.ByteString) IO

-- store files as strict strings?
-- needs profiling
instance StorageMonad Memory where
  writeFile f s = do
    m <- get
    if f `member` m
      then return $ Left NotOverwritng
      else modify' (Map.insert f s) >> return (Right ())

  readFile f = do
    m <- get
    liftIO $
      tryJust
        (\(_ :: ErrorCall) -> Just NotFound)
        (liftIO $ return $ m ! f)

  expandPath p = return $ ":" ++ p ++ ":"

runMemoryStorage :: Memory a -> IO a
runMemoryStorage n = fst <$> runStateT n Map.empty
