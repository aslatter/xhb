-- |The 'XCB' package can parse the contents of the xcb-proto
-- XML files into Haskell data structures.
--
-- Pretty-printers are provided to aid in the debugging - they do
-- not pretty-print to XML, but to a custom human-readable format.

module XCB
    (module XCB.Types
    ,module XCB.FromXML
    ,module XCB.Pretty
    ,declaredTypes
    ) where

import XCB.Types
import XCB.FromXML
import XCB.Pretty

import Control.Applicative
import Data.Maybe

-- Helper funcions

-- TODO: move to Generate group of modules, as it assumes
-- details of how generation is carried out.
-- |A list of all of the type defined by the modue.
declaredTypes :: XHeader -> [Name]
declaredTypes xhd =
    let decls = xheader_decls xhd

        tyName (XStruct name _) = return name
        tyName (XTypeDef name _) = return name
        tyName (XEvent name _ _) = return name
        tyName (XRequest name _ _ Nothing) = return name
        tyName (XRequest name _ _ _) = [name, name ++ "Reply"]
        tyName (XidType name) = return name
        tyName (XidUnion name _) = return name
        tyName (XEnum name _) = return name
        tyName (XUnion name _) = return name
        tyName XImport{} = empty
        tyName (XError name _ _) = return name

    in concatMap tyName decls
