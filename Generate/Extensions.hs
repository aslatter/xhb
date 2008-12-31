module Generate.Extensions where

import Generate.Functions (typesModName, applyMany)
import Generate (decodeFnType)
import Generate.Facts

import HaskellCombinators

import Data.XCB

import Data.Maybe

extensionsModules :: [XHeader] -> [HsModule]
extensionsModules xs =
    let newMod = mkModule extensionsModuleName
        exts = extensions xs
        f = applyMany [ errorDispatch exts
                      , eventDispatch exts
                      , doImports exts
                      ]
    in return $ f newMod

-- | This module generates helper-code for wrking with extensions
-- that doesn't really belong with any single extension.

extensions :: [XHeader] -> [XHeader]
extensions = filter isExtHeader
 where isExtHeader xhd | isJust (xheader_xname xhd) = True
       isExtHeader _ = False

-- | We want to generate a function to decode errors
errorDispatch = dispatchFn False

-- | and another for events
eventDispatch = dispatchFn True


dispatchFn :: Bool -> [XHeader] -> (HsModule -> HsModule)
dispatchFn _ [] = id
dispatchFn event xs = applyMany $ map addDecl $
        [ mkTypeSig fnName [] typ
        , hsFunBind $ map go xs ++ [defaultMatch]
        ]

 where
   typ :: HsType
   typ = mkTyCon "ExtensionId" `hsTyFun` decodeFnType fnRetCon

   fnRetCon | event = "SomeEvent"
            | otherwise = "SomeError"

   fnName | event = "eventDispatch"
          | otherwise = "errorDispatch"

   decodeFn | event = eventDecodeFn
            | otherwise = errorDecodeFn

   go :: XHeader -> HsMatch
   go xhd =
       let extId = fromJust $ xheader_xname xhd
           modName = typesModName xhd
       in mkLitMatch fnName (hsString extId) $
          hsVar $ mkQName modName decodeFn
   
   defaultMatch = mkMatch fnName [hsPWildCard]
                  (foldr1 (\x y -> x `hsApp` hsParen y)
                      [ mkVar "const"
                      , mkVar "const"
                      , mkConExp "Nothing"
                      ]
                  )

doImports :: [XHeader] -> (HsModule -> HsModule)
doImports [] = id
doImports xs = applyMany $ map (addImport) $ otherImports ++ map go xs
 where
   go :: XHeader -> HsImportDecl
   go = mkQualImport . typesModName

   otherImports :: [HsImportDecl]
   otherImports = [mkImport $ packagePrefix ++ ".Shared"
                  ,mkImport "Data.Binary.Get"
                  ,mkImport "Data.Word"
                  ]
