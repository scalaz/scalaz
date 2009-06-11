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

filterRecent :: FilePath -> [FilePath] -> IO [FilePath]
filterRecent d s = if null d
                     then return s
                     else do e <- doesDirectoryExist d
                             if e
                               then let r = getModificationTime d in filterM (\z -> liftM2 (>) (getModificationTime z) (getModificationTime d)) s
                               else do createDirectoryIfMissing True d
                                       return s
