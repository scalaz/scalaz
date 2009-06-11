{-
This belongs in its own library
-}

module Compile(recurse, Compile, (!!!), filterRecent, foldIf, systems) where

import Control.Monad
import System.Cmd
import System.Exit
import System.FilePath.Find hiding (directory) -- http://hackage.haskell.org/packages/archive/FileManip/latest/doc/html/
import System.Directory

class Compile c where
  (!!!) :: c -> [FilePath] -> IO ExitCode

recurse :: (Compile c) => String -> c -> [FilePath] -> IO ExitCode
recurse e c p = do k <- find always (extension ==? e) `mapM` p
                   c !!! concat k

filterRecent :: [FilePath] -> [FilePath] -> IO [FilePath]
filterRecent d s = if null d
                     then return s
                     else let r = fmap maximum $ mapM getModificationTime d in filterM (\z -> liftM2 (>) (getModificationTime z) r) s

foldIf :: (Monad m) => (a -> Bool) -> (t -> m a) -> a -> [t] -> m a
foldIf _ _ e [] = return e; foldIf z k e (h:t) = k h >>= \c -> if z c then e `seq` foldIf z k e t else return c

systems :: [String] -> IO ExitCode
systems = foldIf (== ExitSuccess) system ExitSuccess
