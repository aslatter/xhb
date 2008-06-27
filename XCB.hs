-- |The 'XCB' package can parse the contents of the xcb-proto
-- XML files into Haskell data structures.
--
-- Pretty-printers are provided to aid in the debugging - they do
-- not pretty-print to XML, but to a custom human-readable format.

module XCB
    (module XCB.Types
    ,module XCB.FromXML
    ,module XCB.Pretty
    ) where

import XCB.Types
import XCB.FromXML
import XCB.Pretty
