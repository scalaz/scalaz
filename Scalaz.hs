{-

Depends
* The Haskell Platform http://hackage.haskell.org/platform/
* Lastik http://hackage.haskell.org/package/Lastik
* time http://hackage.haskell.org/package/time


$LastChangedRevision$
$LastChangedDate$
$LastChangedBy$

-}

module Scalaz where

import qualified Lastik.Scala.Scalac as S
import qualified Lastik.Scala.Scaladoc as SD
import Lastik.Runner
import Lastik.Output
import Lastik.Directory
import Lastik.Util
import Lastik.Find
import System.FilePath
import System.Cmd
import System.Directory
import System.Process
import System.Exit
import Control.Monad
import Data.List hiding (find)
import Data.Time.Clock
import Data.Time.Calendar
import Scalaz.Build

s :: FilePath -> S.Scalac
s d = S.scalac {
  S.directory = Just d,
  S.deprecation = True,
  S.classpath = depends,
  S.etc = Nothing -- Just "-Xcheckinit -Xlog-implicits -Xstrict-warnings -Xwarninit"
}

f :: S.Fsc
f = S.fsc {
  S.fscalac = s buildClasses
}

main' :: S.Fsc
main' = f

main :: IO ExitCode
main = main' +->- [mainDir, httpDir]

main'' :: S.Fsc
main'' = main' >=>=> f

example :: IO ExitCode
example = main >>>> (main'' ->- [exampleDir])

{-
test :: IO ExitCode
test = main >>>> (main'' ->- [testDir])
-}

-- todo Update Lastik Scaladoc for Scala 2.8.0
scaladoc' :: Version -> SD.Scaladoc
scaladoc' v = SD.scaladoc {
  SD.directory = Just buildScaladoc,
  SD.etc = Just ("-doc-title \"Scalaz " ++ v ++ " API Specification <div><p><em>Copyright 2008 - 2009 Tony Morris, Runar Bjarnason, Tom Adams, Kristian Domagala, Brad Clow, Ricky Clarkson, Paul Chiusano, Trygve Laugstøl, Nick Partridge, Jason Zaugg</em></p>This software is released under an open source BSD licence.</div>\""),
  SD.classpath = depends
}

scaladoc :: IO ExitCode
scaladoc = do v <- version'
              scaladoc' v ->- [mainDir, httpDir]

-- todo scala function in Lastik
scala :: String -> IO ExitCode
scala k = system ("scala " ++ k)

-- todo jar function in Lastik
jar :: String -> IO ExitCode
jar k = system ("jar " ++ k)

qrunExample :: String -> IO ExitCode
qrunExample e = scala (intercalate " " [cp, "scalaz." ++ e])

runExample :: String -> IO ExitCode
runExample e = example >>>> qrunExample e

allExample :: IO ExitCode
allExample = runExample "Example"

qallExample :: IO ExitCode
qallExample = qrunExample "Example"

repl :: IO ExitCode
repl = example >>>> {- test >>>> -} qrepl

qrepl :: IO ExitCode
qrepl = scala (intercalate " " ["-i initrepl", cp])

clean :: IO ()
clean = rmdir build

sversion :: FilePath -> FilePath -> IO ExitCode
sversion c f = do (ec, o, e) <- readProcessWithExitCode c ["-version"] []
                  writeFile f o
                  appendFile f e
                  return ec

sbuildversion :: FilePath -> FilePath -> IO ExitCode
sbuildversion c f = mkdir build >> sversion c (buildScalaz </> f)

scalaversion :: IO ExitCode
scalaversion = "scala" `sbuildversion` "scalaversion"

scalacversion :: IO ExitCode
scalacversion = "scalac" `sbuildversion` "scalacversion"

scaladocversion :: IO ExitCode
scaladocversion = "scaladoc" `sbuildversion` "scaladocversion"

scalazversion :: IO ()
scalazversion = version' >>= writeFile (buildScalaz </> "scalazversion")

time :: IO ()
time = getCurrentTime >>= \t -> writeFile (buildScalaz </> "time") (show (utctDay t) ++ "+" ++ show (utctDayTime t))

releasetype :: ReleaseType -> IO ()
releasetype t = let r' Release = ("*", " ", " ")
                    r' PreRelease = (" ", "*", " ")
                    r' ReleaseCandidate = (" ", " ", "*")
                    (a, b, c) = r' t
                    z = "[" ++ a ++ "] Release\n[" ++ b ++ "] Pre-release\n[" ++ c ++ "] Release Candidate\n"
                in writeFile (buildScalaz </> "releasetype") z

meta :: IO [ExitCode]
meta = mkdir buildScalaz >> time >> scalazversion >> sequence [scalaversion, scalacversion, scaladocversion]

archive :: IO ExitCode
archive = mkdir buildJar >>
          meta >>
          main >>>>
          example >>>>
          -- test >>>>
          let z = intercalate " " (fmap (\(d, f) -> "-C " ++ d ++ " " ++ f) [(buildScalaz, "scalaversion"), (buildScalaz, "scalazversion"), (buildScalaz, "time"), (buildScalaz, "scalacversion")])
              r = "-cvfm " ++ buildJar' ++ " " ++ resourcesDir </> "META-INF" </> "MANIFEST.MF " ++ z ++ " -C " ++ buildClasses ++ " ."
          in jar r

data ReleaseType = Release | PreRelease | ReleaseCandidate deriving (Eq, Show)

release :: ReleaseType -> IO ExitCode
release t = let c = copyFiles nosvn nosvnf
            in do clean
                  scaladoc
                  archive
                  releasetype t
                  c mainDir (buildScalaz </> "src")
                  -- c testDir (buildScalaz </> "test")
                  c exampleDir (buildScalaz </> "example")
                  c etcDir buildScalaz
                  mkdir buildRelease
                  jar ("-cvfM " ++ buildRelease </> "scalaz.zip" ++ " -C " ++ build ++ " scalaz")

svn :: String -> IO ExitCode
svn k = system ("svn " ++ k)

-- todo
pre :: IO ExitCode
pre = do release PreRelease
         t <- readFile (buildScalaz </> "time")
         svn ("import " ++ buildScalaz ++ " " ++ continuous ++ '/' : t  ++ " -m " ++ commitMessage)


nosvn :: FilePather Bool
nosvn = fileName /=? ".svn"

nosvnf :: FilterPredicate
nosvnf = constant nosvn ?&&? isFile
