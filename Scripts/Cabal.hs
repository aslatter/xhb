module Cabal where

import Text.StringTemplate

import System.IO
import System.Environment (getArgs, getEnv)
import System.Exit
import System.Locale
import System.FilePath

import Data.Time
import qualified Data.List as L

import Generate.Functions (typesModName, functionsModName)
import Generate.Facts (otherModuleNames)

import qualified Data.XCB as XCB

{-

  plugs the generated files into a cabal-file template

-}


{-
 args ! 0    -- cabal file template
 args ! 1    -- output file name
 args ! 2    -- directory containing generated files
 args ! 3    -- xprotot version
 drop 4 args -- names of generated modules
 -}

templateFile = "Templates" </> "cabal.template"
genDirName = "patched"

main = do
  args <- getArgs
  if length args < 1 then exitBadArgs else do
  let outDir : xmlFileNames = args
      outFile = outDir </> "cabalize.cabal"

  headers <- XCB.fromFiles xmlFileNames
  templateString <- readFile templateFile
  time <- Just `fmap` getCurrentTime
  version <- getEnv "XPROTO_VERSION"
  let cabalString =
          applyTemplate time templateString genDirName version headers
  writeFile outFile cabalString

exitBadArgs = do
  hPutStrLn stderr "Command requires at least one arguments."
  exitFailure

applyTemplate :: Maybe UTCTime -> String -> String
              -> String -> [XCB.XHeader] -> String
applyTemplate time template dirName version xhds =
    toString $ setAttribute "Module" (map functionsModName xhds) $
               setAttribute "OtherModule" (map typesModName xhds) $
               setAttribute "OtherModule" otherModuleNames $
               setAttribute "GenDir" dirName $
               setAttribute "DateString" (dateString `fmap` time) $
               setAttribute "XProtoVersion" version $
               newSTMP template

dateString :: UTCTime -> String
dateString time = let
    format str = formatTime defaultTimeLocale str time
                 
    stripZero ('0':xs) = stripZero xs
    stripZero xs = xs

    yearString = format "%Y"
    monthString = stripZero $ format "%m"
    dayString = stripZero $ format "%d"
 in concat $ L.intersperse "." [yearString, monthString, dayString]