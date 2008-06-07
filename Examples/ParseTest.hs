{-
  The name of an XML file is specified as the first argument
  to this program.  Specifying zero or more than one arguments
  is an error.

  The XML file is parsed as if it were one of the X Protocol
  description files used by XCB.

  The internal data-structures created from the parsing are
  pretty-printed to standard output.

  Antoine Latter
  aslatter@gmail.com
 -}


import FromXML
import Pretty
import Types

import System.IO
import System.Environment
import System.Exit

fromFile :: FilePath -> IO XHeader
fromFile fp = do
  text <- readFile fp
  return $ fromString text

main = do
  fp <- getOneArg
  xheader <- fromFile fp
  print $ toDoc xheader

getOneArg = catch (do {[arg] <- getArgs; return arg})
                  (const (do {putStrLn "Please call with a single argument";
                              exitFailure}))
