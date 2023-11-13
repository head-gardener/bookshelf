module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans
import Control.Monad.Trans.Resource (runResourceT)
import Data.Entities as ES
import Data.Text (pack)
import Data.Time
import Database.Persist.Sqlite
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ("--db" : db : rs) = runStderrLoggingT $
  withSqlitePool (pack db) 10 $
    \pool -> liftIO $ parseArgs_ (Just pool) rs
parseArgs args = runStderrLoggingT $
  withSqlitePool "test.db3" 10 $
    \pool -> liftIO $ parseArgs_ (Just pool) args

parseArgs_ _ ("--db" : _ : _) = putStrLn "Duplicate db specfication"
parseArgs_ (Just pool) ["add", title, content] = do
  putStrLn "adding verse..."
  k <- runSql pool . insert . Verse (pack title) (pack content) Nothing =<< getCurrentTime
  print k
parseArgs_ (Just pool) ["repopulate"] = do
  putStrLn "repopulating..."
  runSql pool populate
parseArgs_ _ _ = do
  putStrLn "usage: manager [--db <db>]"
  putStrLn "\tadd ..."
  putStrLn "\trepopulate"

runSql pool f =
  runResourceT $
    flip runSqlPool pool $
      runMigration migrateAll >> f
