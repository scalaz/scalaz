{-

$LastChangedRevision$
$LastChangedDate$
$LastChangedBy$

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

exampleDir = "example"  </> "src" </> "main" </> "scala"
mainDir = "core"  </> "src" </> "main" </> "scala"
httpDir = "http"  </> "src" </> "main" </> "scala"
--scappsDir = "scapps"  </> "src" </> "main" </> "scala"
-- testDir = "core"  </> "src" </> "test" </> "scala"
resourcesDir = "resources"
etcDir = "etc"

build = "build"
buildClasses = build </> "classes"
buildScalaz = build </> "scalaz"
buildScaladoc = buildScalaz </> "scaladoc"
buildJar = buildScalaz
jarFile = "scalaz.jar"
buildJar' = buildJar </> jarFile
buildRelease = build </> "release"

type Version = String

version' :: IO Version
version' = readFile "version"

cp :: String
cp = "classpath" ~?? [buildClasses]

depends :: [String]
depends = ["servlet-api-2.5.jar"]
