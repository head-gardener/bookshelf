{-# OPTIONS_GHC -Wno-orphans #-}

module System.Storage.Memory where

import Control.Monad.State
import Data.ByteString.Lazy qualified as BL
import System.Storage
import Data.Map as Map

type Memory = StateT (Map FilePath BL.ByteString) IO

-- store files as strict strings?
-- needs profiling
instance StorageMonad Memory where
  writeFile f s = do
    m <- get
    when (f `member` m) $ error "File already there :("
    modify' $ Map.insert f s

  readFile f = gets (! f)

  expandPath p = return $ ":" ++ p ++ ":"

runMemoryStorage :: Memory a -> IO a
runMemoryStorage n = fst <$> runStateT n Map.empty
