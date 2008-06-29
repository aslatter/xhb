
import Data.XCB
import Generate

import System.IO
import System.Environment

import System.FilePath

import Language.Haskell.Syntax
import Language.Haskell.Pretty

main = do
  fs <- getArgs
  xheaders <- fromFiles fs
  writeModules xheaders

writeModules :: [XHeader] -> IO ()
writeModules = sequence_ . map writeModule . toHsModules

writeModule :: HsModule -> IO ()
writeModule hsmod = 
    let modname = getHsModName hsmod
        outString = prettyPrint hsmod
    in writeFile (outdir </> modname <.> "hs") outString

getHsModName :: HsModule -> String
getHsModName (HsModule _ mod _ _ _) = prettyPrint mod

outdir = "generated"

