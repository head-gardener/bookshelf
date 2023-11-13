module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Entities as ES
import Database.Persist.Sqlite
import Yesod
import Foundation
import Application ()
import System.Exit (exitFailure)
import System.Environment (getArgs)

main :: IO ()
main = do
  port <- getArgs >>= parseArgs
  runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    warp port $ BookShelf pool
  where
    parseArgs [port] = return $ read port
    parseArgs [] = return 3000
    parseArgs _ = putStrLn "unexpected extra args" >> exitFailure
