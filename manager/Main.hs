module Main where

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans
import Control.Monad.Trans.Resource (runResourceT)
import Data.Entities as ES
import Data.Text (pack)
import Database.Persist.Sqlite
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ("--db" : db : rs) = runNoLoggingT $
  withSqlitePool (pack db) 10 $
    \pool -> liftIO $ parseArgs_ (Just pool) rs
parseArgs args = runNoLoggingT $
  withSqlitePool "test.db3" 10 $
    \pool -> liftIO $ parseArgs_ (Just pool) args

parseArgs_ _ ("--db" : _ : _) = putStrLn "Duplicate db specfication"
parseArgs_ (Just pool) ["verse", t, content] = do
  putStrLn "adding verse..."
  k <-
    runSql pool $
      newEntry $
        Verse (pack t) (pack content) Nothing
  print k
parseArgs_ (Just pool) ["upload", t, path] = do
  putStrLn "uploading..."
  k <- newFile (pack t) path >>= runSql pool . newEntry
  print k
parseArgs_ (Just pool) ["repopulate"] = do
  putStrLn "repopulating..."
  runSql pool populate
parseArgs_ _ _ = do
  putStrLn "usage: manager [--db <db>]"
  putStrLn "\tverse ..."
  putStrLn "\tupload ..."
  putStrLn "\trepopulate"

runSql pool f =
  runResourceT $
    flip runSqlPool pool $
      runMigration migrateAll >> f
