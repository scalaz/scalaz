module Compile where

import System.Exit
import System.FilePath.Find -- -- http://hackage.haskell.org/packages/archive/FileManip/latest/doc/html/

class Compile c where
  (!!!) :: c -> [FilePath] -> IO ExitCode

recurse :: (Compile c) => String -> c -> [FilePath] -> IO ExitCode
recurse e c p = do k <- find always (extension ==? e) `mapM` p
                   c !!! concat k