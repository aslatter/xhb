
import Data.XCB

import Generate
import Generate.Functions(functionsModules)
import Generate.Extensions(extensionsModules)

import System.IO
import System.Environment

import System.FilePath
import System.Directory

import Data.List

import Language.Haskell.Syntax
import Language.Haskell.Pretty

main = do
  out:fs <- getArgs
  xheaders <- fromFiles fs
  let modules = mkModules xheaders
  writeModules out modules

mkModules :: [XHeader] -> [HsModule]
mkModules xhds = toHsModules xhds ++ functionsModules xhds
                 ++ extensionsModules xhds

writeModules :: FilePath -> [HsModule] -> IO ()
writeModules out = sequence_ . map (writeModule out)

writeModule :: FilePath -> HsModule -> IO ()
writeModule outdir hsmod = 
    let modname = getHsModName hsmod
        outString = prettyPrint hsmod
        fileName = baseModule modname <.> "hs"
        dirName = (outdir </>) $ replace '.' '/' $ nameSpace modname
    in if null dirName
       then writeFile fileName outString
       else do
         createDirectoryIfMissing True dirName
         writeFile (dirName </> fileName) outString

getHsModName :: HsModule -> String
getHsModName (HsModule _ mod _ _ _) = prettyPrint mod

-- | Applying to "A.B.C" returns "C"
baseModule :: String -> String
baseModule name =
    let dots = findIndices (=='.') name
        base = drop (last dots + 1) name
    in if null dots then name else base

-- | Applying to "A.B.C" returns "A.B"
nameSpace :: String -> String
nameSpace name =
    let dots = findIndices (=='.') name
        spaceName = take (last dots) name
    in if null dots then "" else spaceName

replace c1 c2 = map go
    where go c | c == c1   = c2
               | otherwise = c
