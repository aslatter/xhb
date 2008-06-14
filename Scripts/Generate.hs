
import Proto
import FromXML
import Types

import System.IO
import System.Environment

import System.FilePath

import Language.Haskell.Syntax
import Language.Haskell.Pretty

pretty :: XHeader -> String
pretty (XHeader nm _ decls) = prettyBuild nm $ mapM_ xDecl decls

main = do
  fs <- getArgs
  xheaders <- fromFiles fs
  writeModules xheaders

writeModules :: [XHeader] -> IO ()
writeModules = sequence_ . map writeModule

writeModule :: XHeader -> IO ()
writeModule xhd = 
    let hsmod = toHsModule xhd
        modname = getHsModName hsmod
        outString = prettyPrint hsmod
    in writeFile (outdir </> modname <.> "hs") outString

getHsModName :: HsModule -> String
getHsModName (HsModule _ mod _ _ _) = prettyPrint mod

outdir = "generated"

