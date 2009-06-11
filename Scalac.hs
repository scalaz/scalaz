{-
This belongs in its own library
-}

module Scalac(none,
              source,
              line,
              vars,
              notailcalls,
              jvm15,
              jvm14,
              msil,
              debug,
              nowarn,
              verbose,
              deprecation,
              unchecked,
              classpath,
              sourcepath,
              bootclasspath,
              extdirs,
              Scalac.directory,
              encoding,
              target,
              Scalac.print,
              optimise,
              explaintypes,
              uniqid,
              version,
              help,
              (#),
              scalac,
              scalacd,
              scalac',
              scalacd',
              ScalacDebug,
              ScalacTarget,
              Scalac,
              Scalac',
              (!*!)) where

import Compile
import System.Cmd
import System.Exit
import System.FilePath
import Data.Char
import Data.List

data ScalacDebug = None | Source | Line | Vars | NoTailCalls
  deriving Eq

none :: ScalacDebug
none = None

source :: ScalacDebug
source = Source

line :: ScalacDebug
line = Line

vars :: ScalacDebug
vars = Vars

notailcalls :: ScalacDebug
notailcalls = NoTailCalls

instance Show ScalacDebug where
  show None = "none"
  show Source = "source"
  show Line = "line"
  show Vars = "vars"
  show NoTailCalls = "notailcalls"

data ScalacTarget = JVM1_5 | JVM1_4 | MSIL
  deriving Eq

jvm15 :: ScalacTarget
jvm15 = JVM1_5

jvm14 :: ScalacTarget
jvm14 = JVM1_4

msil :: ScalacTarget
msil = MSIL

instance Show ScalacTarget where
  show JVM1_5 = "jvm-1.5"
  show JVM1_4 = "jvm-1.4"
  show MSIL = "msil"

-- todo support -X options
data Scalac = Scalac {
  debug :: Maybe ScalacDebug,
  nowarn :: Bool,
  verbose :: Bool,
  deprecation :: Bool,
  unchecked :: Bool,
  classpath :: Maybe [FilePath],
  sourcepath :: Maybe [FilePath],
  bootclasspath :: Maybe [FilePath],
  extdirs :: Maybe [FilePath],
  directory :: Maybe FilePath,
  encoding :: Maybe String,
  target :: Maybe ScalacTarget,
  print :: Bool,
  optimise :: Bool,
  explaintypes :: Bool,
  uniqid :: Bool,
  version :: Bool,
  help :: Bool,
  (#) :: Maybe FilePath
}

scalac :: Scalac
scalac = Scalac Nothing False False False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing False False False False False False Nothing

scalacd :: Scalac
scalacd = scalac {
                   debug = Just vars,
                   verbose = True,
                   deprecation = True,
                   unchecked = True,
                   explaintypes = True
                 }

-- not exported
dscalac (Scalac debug
                nowarn
                verbose
                deprecation
                unchecked
                classpath
                sourcepath
                bootclasspath
                extdirs
                directory
                encoding
                target
                print
                optimise
                explaintypes
                uniqid
                version
                help
                script) = intercalate " " $ filter (not . null) ["scalac",
                          c "g" show debug,
                          b "nowarn" nowarn,
                          b "verbose" verbose,
                          b "deprecation" deprecation,
                          b "unchecked" unchecked,
                          p "classpath" classpath,
                          p "sourcepath" sourcepath,
                          p "bootclasspath" bootclasspath,
                          p "extdirs" extdirs,
                          s "d" id directory,
                          s "encoding" id encoding,
                          s "target" show target,
                          b "print" print,
                          b "optimise" optimise,
                          b "explaintypes" explaintypes,
                          b "uniqid" uniqid,
                          b "version" version,
                          b "help" help,
                          m ((:) '@') script]
                          where
                          d = (:) '-'
                          b :: String -> Bool -> String
                          b = b' . d
                          b' :: [a] -> Bool -> [a]
                          b' s k = if k then s else []
                          m :: (k -> [a]) -> Maybe k -> [a]
                          m = maybe []
                          c :: String -> (k -> String) -> Maybe k -> String
                          c = v ':'
                          s :: String -> (k -> String) -> Maybe k -> String
                          s = v ' '
                          v :: Char -> String -> (k -> String) -> Maybe k -> String
                          v c k s = m (\z -> '-' : k ++ [c] ++ s z)
                          p :: String -> Maybe [FilePath] -> String
                          p s = m (\z -> d s ++ ' ' : if null z then "\"\"" else intercalate [searchPathSeparator] (fmap surround z))

-- not exported
surround s = '"' : s ++ ['"']

-- not exported
doscalac k s ps = let v = dscalac s ++ ' ' : intercalate " " (fmap surround ps) in do k v >> system v

instance Compile Scalac where
  s !!! ps = doscalac (const $ return ()) s ps

newtype Scalac' = Scalac' Scalac

instance Compile Scalac' where
  Scalac' s !!! ps = doscalac (Prelude.print) s ps

scalac' = Scalac'

(!*!) :: (Compile c) => c -> [FilePath] -> IO ExitCode
(!*!) = recurse ".scala"

scalacd' = scalac' scalacd