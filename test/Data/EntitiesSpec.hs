module Data.EntitiesSpec where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Entities
import Data.String ()
import Database.Persist.Sqlite
import Test.Hspec
import Data.Maybe (isJust)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "entities" $ do
    it "can be stored" $ do
      res <- runDB $ do
        key <- newEntry $ Verse "hey" "" Nothing
        get key
      fmap title res `shouldBe` Just "hey"

    it "can be chain updated" $ do
      (updated1, v1, v2, f1, f2) <- runDB $ do
        f1 <- newEntry $ File "file1" "1" ""
        Just v1 <- get =<< newEntry (Verse "verse1" "" (Just f1))
        -- first update returns Nothing
        update1 <- verseChainUpdate v1

        Just f1' <- get f1
        f2 <- editEntry (versionRoot f1') $ File "file2" "2" ""
        Just update2 <- verseChainUpdate v1
        Just v2 <- get =<< update2
        return (isJust update1, verseFile v1, verseFile v2, f1, f2)

      updated1 `shouldBe` False
      v1 `shouldBe` Just f1
      v2 `shouldBe` Just f2

runDB :: ReaderT SqlBackend (ResourceT (LoggingT IO)) a -> IO a
runDB a =
  runStderrLoggingT $
    filterLogger (\_ l -> l > LevelError) $
      withSqlitePool ":memory:" 10 $
        runResourceT . runSqlPool (runMigrationQuiet migrateAll >> a)
