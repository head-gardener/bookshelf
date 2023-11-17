module System.StorageSpec where

import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.Time
import System.Storage as ST
import System.Storage.Memory
import System.Storage.Native
import Test.Hspec
import Test.QuickCheck
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "storage" $ do
    it "stores files" $
      property $ do
        memoryShouldReturn (Right "test") $ do
          written <- ST.writeFile "testFile" "test"
          either (return . Left) (const $ ST.readFile "testFile") written
        nativeShouldReturn (Right "test") $ do
          written <- ST.writeFile "testFile" "test"
          either (return . Left) (const $ ST.readFile "testFile") written

    it "fails on invalid reads" $ do
      property $ do
        memoryShouldReturn
          (Left NotFound)
          (liftIO $ runMemoryStorage $ ST.readFile "testFile")
        nativeShouldReturn
          (Left NotFound)
          (liftIO $ runMemoryStorage $ ST.readFile "testFile")

memoryShouldReturn :: (Show a, Eq a) => a -> Memory a -> IO ()
memoryShouldReturn r a = do
  res <- liftIO $ runMemoryStorage a
  res `shouldBe` r

nativeShouldReturn :: (Show a, Eq a) => a -> Native a -> IO ()
nativeShouldReturn r a = do
  path <- liftIO $ fmap format getCurrentTime
  createDirectoryIfMissing True path
  res <- liftIO $ runNativeStorage a path
  res `shouldBe` r
  where
    format = ("./test-files/" ++) . intercalate "=" . words . show
