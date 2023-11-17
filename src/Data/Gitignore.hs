module Data.Gitignore (applyGitignore, applyGitignoreL) where

import Data.List
import System.FilePath

-- | Simple gitignore approximation.
-- Handles most meaningful cases.
-- Requires full file tree as input.
applyGitignore :: [String] -> [FilePath] -> [FilePath]
applyGitignore ignore = filter (not . matchesGitignore stripped)
  where stripped = stripGI ignore

-- | Lazy applyGitignore.
-- Currently doesn't work, but should olny
-- ls dirs when needed.
applyGitignoreL :: [String] -> [FilePath] -> IO [FilePath]
applyGitignoreL i fs = return $ applyGitignore i fs

stripGI :: [[Char]] -> [[Char]]
stripGI = filter (('#' /=) . head)

matchesGitignore :: [String] -> FilePath -> Bool
matchesGitignore gitignoreContent filePath =
  any (`matchesPattern` fs) ps
  where
    fs = splitDirectories filePath
    ps = fmap splitDirectories gitignoreContent

matchesPattern :: [String] -> [FilePath] -> Bool
matchesPattern [] _ = True
matchesPattern _ [] = False
matchesPattern (p : ps) (f : fs) =
  matchesSegment p f && matchesPattern ps fs

matchesSegment :: String -> String -> Bool
matchesSegment p s
  | p == "" = True
  | head p == '*' && last p == '*' = (tail . init $ p) `isInfixOf` s
  | head p == '*' = tail p `isSuffixOf` s
  | last p == '*' = init p `isPrefixOf` s
  | otherwise = p == s
