
import Proto
import FromXML
import Types

import System.IO
import System.Environment

fromFile :: FilePath -> IO XHeader
fromFile fp = do
  text <- readFile fp
  return $ fromString text

pretty :: XHeader -> String
pretty (XHeader nm _ decls) = prettyBuild nm $ mapM_ xDecl decls

main = do
  [fp] <- getArgs
  xheader <- fromFile fp
  putStrLn $ pretty xheader
