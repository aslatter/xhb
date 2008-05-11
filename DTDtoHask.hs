module DTDtoHask where

import Text.XML.HaXml
import Text.XML.HaXml.DtdToHaskell.Convert
import Text.XML.HaXml.DtdToHaskell.Instance
import Text.XML.HaXml.DtdToHaskell.TypeDef
import System.Environment
import Text.PrettyPrint.HughesPJ
import Data.Char

main = do
  [dtdFileName] <- getArgs
  dtdText <- readFile dtdFileName
  let DTD dtdName exId markup
          = case dtdParse dtdText dtdFileName of
              Nothing -> error "Could not parse DTD"
              Just a -> a
  putStrLn $ "module " ++ ensureUpper dtdName ++ " where\n"
  let defs = dtd2TypeDef markup
  print . hsep . map ppTypeDef $ defs
  putStrLn "\n"
  print . hsep $ map mkInstance defs

ensureUpper [] = []
ensureUpper (x:xs) = toUpper x : xs