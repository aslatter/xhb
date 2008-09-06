
module Generate.Util where

import Data.Char

ensureUpper :: String -> String
ensureUpper [] = []
ensureUpper (x:xs) = (toUpper x) : xs
