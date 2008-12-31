module Generate.Facts where

import Data.XCB
import Data.Char

import Generate.Util

packagePrefix = "Graphics.XHB"

modulesPrefix = packagePrefix ++ ".Gen"

conPrefix = ("Mk" ++)

typesModuleName modName = modulesPrefix ++ "." ++ modName ++ ".Types"
functionsModuleName modName = modulesPrefix ++ "." ++ modName

extensionsModuleName = modulesPrefix ++ "." ++ "Extension"

otherModuleNames = [extensionsModuleName]

accessor field typ = field ++ "_" ++ typ
replyName = (++ "Reply")

errorDecodeFn = "deserializeError"
eventDecodeFn = "deserializeEvent"

modName :: XHeader -> Name
modName xhd = case xheader_name xhd of
                Nothing -> ensureUpper $ xheader_header xhd
                Just name -> name

