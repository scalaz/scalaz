module Scalaz where

import Control.Applicative
import Data.Time.Clock
import Data.Traversable
import System.Process
import System.IO
import System.Exit
import System.Directory
import System.FilePath
import Prelude hiding (mapM)

data Config = Config {
  actions :: [String],
  outputLog :: Maybe String,
  command :: String,
  modules :: [String]
}

defaultConfig :: Config
defaultConfig = Config {
  actions = ["test", "package-all"],
  outputLog = Just "build.log",
  command = "sbt",
  modules = ["core", "example", "http", "scalacheck-binding", "tests"]
}

sbt' :: Config -> IO ExitCode
sbt' c = do h <- flip openFile WriteMode `mapM` outputLog c
            d <- getCurrentDirectory
            p <- runProcess (d </> command c) (actions c) (Just d) Nothing Nothing h Nothing
            e <- waitForProcess p
            maybe (return ()) hClose h
            return e

sbt :: IO ExitCode
sbt = sbt' defaultConfig

time :: IO String
time = (\t -> (show (utctDay t) ++ "+" ++ show (utctDayTime t))) <$> getCurrentTime
