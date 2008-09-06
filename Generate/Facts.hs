module Generate.Facts where

import Data.XCB
import Data.Char

import Generate.Util

modulesPrefix = "XHB.Gen"

conPrefix = ("Mk" ++)
typesModuleName modName = modulesPrefix ++ "." ++ modName ++ ".Types"
functionsModuleName modName = modulesPrefix ++ "." ++ modName
accessor field typ = field ++ "_" ++ typ
replyName = (++ "Reply")


modName :: XHeader -> Name
modName xhd = case xheader_name xhd of
                Nothing -> ensureUpper $ xheader_header xhd
                Just name -> name

