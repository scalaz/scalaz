module Scalaz where

import System.Process
import System.IO
import System.Exit
import System.Directory
import System.FilePath

data Config = Config {
  actions :: [String],
  outputLog :: String,
  command :: String
}

defaultConfig :: Config
defaultConfig = Config {
  actions = ["test", "package-all"],
  outputLog = "build.log",
  command = "sbt"
}

sbt' :: Config -> IO ExitCode
sbt' c = do h <- openFile (outputLog c) WriteMode
            d <- getCurrentDirectory
            p <- runProcess (d </> command c) (actions c) (Just d) Nothing Nothing (Just h) Nothing
            e <- waitForProcess p
            hClose h
            return e

sbt :: IO ExitCode
sbt = sbt' defaultConfig
