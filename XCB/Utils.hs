module XCB.Utils where

-- random utility functions

import Data.Char

ensureUpper :: String -> String
ensureUpper [] = []
ensureUpper (x:xs) = (toUpper x) : xs
