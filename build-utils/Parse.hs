{-
  The XML files are parsed as if they were X Protocol
  description files used by XCB.

  The internal data-structures created from the parsing are
  pretty-printed to a file per input file.

  Antoine Latter
  aslatter@gmail.com
 -}


import Data.XCB

import System.IO
import System.Environment
import System.Exit

import System.FilePath

main = do
  out:fps <- getArgs
  xheaders <- fromFiles fps
  writeHeaders out xheaders

writeHeaders :: FilePath -> [XHeader] -> IO ()
writeHeaders out = sequence_ . map (writeHeader out)

writeHeader :: FilePath -> XHeader -> IO ()
writeHeader outdir xhd =
    let fname = outdir </> xname <.> "out"
        xname = xheader_header xhd
        outString = pretty xhd
    in writeFile fname outString

