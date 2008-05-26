
import Proto
import FromXML
import Types
import Pretty

import System.IO
import System.Environment

fromFile :: FilePath -> IO XHeader
fromFile fp = do
  text <- readFile fp
  return $ fromString fp text

main = do
  [fp] <- getArgs
  xheader <- fromFile fp
  print $ toDoc xheader
