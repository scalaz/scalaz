{-

$LastChangedRevision: 1348 $
$LastChangedDate: 2010-01-07 20:28:23 +1000 (Thu, 07 Jan 2010) $
$LastChangedBy: tonymorris $

-}

module Scalaz.Build where

import System.FilePath
import Lastik.Util

commitMessage :: String
commitMessage = "\"Automated build (Scalaz.hs)\""

repo :: String
repo = "https://scalaz.googlecode.com/svn"

repo' :: String -> String
repo' = (repo ++) . ('/' :)

continuous :: String
continuous = repo' "continuous"

trunk :: String
trunk = repo' "trunk"

artifacts :: String
artifacts = repo' "artifacts"

exampleDir :: FilePath
exampleDir = "example"  </> "src" </> "main" </> "scala"

mainDir :: FilePath
mainDir = "core"  </> "src" </> "main" </> "scala"

httpDir :: FilePath
httpDir = "http"  </> "src" </> "main" </> "scala"

--scappsDir = "scapps"  </> "src" </> "main" </> "scala"
-- testDir = "core"  </> "src" </> "test" </> "scala"

resourcesDir :: FilePath
resourcesDir = "resources"

etcDir :: FilePath
etcDir = "etc"

build :: FilePath
build = "build"

buildClasses :: FilePath
buildClasses = build </> "classes"

buildScalaz :: FilePath
buildScalaz = build </> "scalaz"

buildScaladoc :: FilePath
buildScaladoc = buildScalaz </> "scaladoc"

buildJar :: FilePath
buildJar = buildScalaz

jarFile :: FilePath
jarFile = "scalaz.jar"

buildJar':: FilePath
buildJar' = buildJar </> jarFile

buildRelease :: FilePath
buildRelease = build </> "release"

type Version = String

version' :: IO Version
version' = readFile "version"

cp :: String
cp = "classpath" ~?? [buildClasses]

depends :: [String]
depends = ["servlet-api-2.5.jar"]
