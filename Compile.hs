{-
This belongs in its own library
-}

module Compile(recurse, Compile, (!!!), filterRecent) where

import Control.Monad
import System.Exit
import System.FilePath.Find hiding (directory) -- http://hackage.haskell.org/packages/archive/FileManip/latest/doc/html/
import System.Directory

class Compile c where
  (!!!) :: c -> [FilePath] -> IO ExitCode

recurse :: (Compile c) => String -> c -> [FilePath] -> IO ExitCode
recurse e c p = do k <- find always (extension ==? e) `mapM` p
                   c !!! concat k

-- p must not non-empty, all filepaths must exist with the ability to get the modification-time
filterRecent :: [FilePath] -> [FilePath] -> IO [FilePath]
filterRecent p q = let r = fmap maximum $ mapM getModificationTime p in filterM (\z -> liftM2 (>) (getModificationTime z) r) q
