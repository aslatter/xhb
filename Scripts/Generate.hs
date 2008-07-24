
import Data.XCB
import Generate

import System.IO
import System.Environment

import System.FilePath

import Language.Haskell.Syntax
import Language.Haskell.Pretty

main = do
  out:fs <- getArgs
  xheaders <- fromFiles fs
  writeModules out xheaders

writeModules :: FilePath -> [XHeader] -> IO ()
writeModules out = sequence_ . map (writeModule out) . toHsModules

writeModule :: FilePath -> HsModule -> IO ()
writeModule outdir hsmod = 
    let modname = getHsModName hsmod
        outString = prettyPrint hsmod
    in writeFile (outdir </> modname <.> "hs") outString

getHsModName :: HsModule -> String
getHsModName (HsModule _ mod _ _ _) = prettyPrint mod

