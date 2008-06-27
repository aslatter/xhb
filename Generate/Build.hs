{-# LANGUAGE FlexibleContexts #-}

module Generate.Build where

import Generate.Types
import XCB
import XCB.Utils
import HaskellCombinators
import BuildData

import qualified Data.Map as M
import Language.Haskell.Syntax

import Control.Monad.RW
import Control.Monad.Reader
import Control.Monad.Writer

import Control.Applicative

import qualified Data.List as L

{-
  Functions in the 'Generate' monad
-}


conPrefix = ("Mk" ++)
modulePrefix = ("XHB.Gen." ++)
accessor field typ = field ++ "_" ++ typ

-- reader accessors

current :: MonadReader ReaderData m => m XHeader
current = return readerData_current `ap` ask

allModules :: MonadReader ReaderData m => m [XHeader]
allModules = return readerData_all `ap` ask

oneModule :: MonadReader ReaderData m => Name -> m (Maybe XHeader)
oneModule name = do
  xs <- allModules
  return $ L.find (hasName name) xs

-- fancyTypeName :: Type -> Generate String
fancyTypeName (UnQualType name) = return name
fancyTypeName (QualType qual name) = do
  qname <- fancyName qual
  case qname of
    Just n -> return $ modulePrefix $ n ++ "." ++ name
    Nothing -> return $ ensureUpper qual ++ "." ++ name

-- fancyName :: Name -> Generate (Maybe Name)
fancyName "xproto" = return $ return "Xproto" -- special case
fancyName str = do
   mod <- oneModule str
   return $ do
     header <- mod
     return $ modName header

hasName str xhd = xheader_header xhd == str



modName :: XHeader -> Name
modName xhd = case xheader_name xhd of
                Nothing -> ensureUpper $ xheader_header xhd
                Just name -> name


--

modifyModule :: (HsModule -> HsModule) -> Gen
modifyModule = tell . buildHsModule

logEvent :: EventName -> Int -> Gen
logEvent  name code =  tell $ buildEvent name code

logRequest :: RequestName -> Int -> Bool -> Gen
logRequest name code hasReply = tell $ buildRequest name code hasReply

logError :: ErrorName -> Int -> Gen
logError name code = tell $ buildError name code


runGenerate :: Name -> ReaderData -> Generate a -> (a, HsModule)
runGenerate nm r m =
    let (x, bdata) = runRW m r
        newModule = fModExtras $ newXhbModule name
        BuildResult mod mapEvent mapReq mapErr = applyBuildData bdata newModule
        name = ensureUpper nm

        fModExtras = mkEventType name mapEvent . 
                     mkRequestType name mapReq .
                     mkErrorType name mapErr
    in (x, mod)

runGen :: Name -> ReaderData -> Gen -> HsModule
runGen n r m = snd $ runGenerate n r m

newXhbModule :: String -> HsModule
newXhbModule = addStandardImports . mkModule . modulePrefix
    where addStandardImports = appMany $ map (addImport . mkImport)
              ["XHB.Shared"
              ,"Data.Word"
              ,"Foreign.C.Types"
              ,"Data.Bits"
              ,"Data.Binary.Put"
              ,"Data.Binary.Get"
              ,"Control.Monad"
              ,"Control.Exception"
              ,"Data.List"
              ]

-- takes all of the results of running 'logEvent' and turns it into to two things:
--   * a sum-type of all the events in this module
--   * a function mapping that sum-type to it's event opcode
--
-- hopefully we can auto-generate srialization/desrialization here as well
mkEventType :: String -- Module name
            -> M.Map EventName Int
            -> HsModule
            -> HsModule
mkEventType _ namesMap | M.null namesMap = id
mkEventType name namesMap = 

    let tyName = name ++ "Event"

        -- Function mapping a module event to its event code
        toCodeFunName = "toEventCode"
        toCodeFnTyp = mkTypeSig toCodeFunName [] (HsTyFun (mkTyCon tyName) (mkTyCon "Int"))
        toCodeFnDec = HsFunBind $ map go $ M.toList namesMap
            where go (typName, code) = 
                      let cons = typName -- the super-type uses the base typenames as constructors
                          result = mkNumLit $ fromIntegral code
                      in mkConsMatch toCodeFunName cons result

        -- Type of any event in this module
        eventTypDec = mkDataDecl [] tyName [] dataCons []
            where dataCons :: [HsConDecl]
                  dataCons = map f $ M.keys namesMap

                  f :: String -> HsConDecl
                  f eventTyName = mkCon eventTyName [HsUnBangedTy $ mkTyCon eventTyName]

        eventExports :: [HsExportSpec]
        eventExports = [mkExportAll tyName
                       --,mkExportAbs toCodeFunName
                       ]

    in appMany (map addExport eventExports)
           -- . addDecl toCodeFnDec
           -- . addDecl toCodeFnTyp
           . addDecl eventTypDec

mkRequestType :: String -- Module name
              -> M.Map RequestName (Int,Bool)
              -> HsModule
              -> HsModule
mkRequestType _ namesMap | M.null namesMap = id
mkRequestType name namesMap =
    let tyName = name ++ "Request"

        toCodeFunName = "toRequestCode"
        toCodeFnTyp = mkTypeSig toCodeFunName [] (HsTyFun (mkTyCon tyName) (mkTyCon "Int"))
        toCodeFnDec = HsFunBind $ map go $ M.toList namesMap
            where go (typName, (code,_)) =
                      let cons = typName
                          result = mkNumLit $ fromIntegral code
                      in mkConsMatch toCodeFunName cons result

        hasRepFunName = "hasReply"
        hasRepFnTyp = mkTypeSig hasRepFunName [] (HsTyFun (mkTyCon tyName) (mkTyCon "Bool"))
        hasRepFnDec = HsFunBind $ map go $ M.toList namesMap
            where go (typName, (_,hasReply)) =
                      let cons = typName
                          result = HsCon $ mkUnQName $ show hasReply
                      in mkConsMatch hasRepFunName cons result


        requestTypDec = mkDataDecl [] tyName [] dataCons []
            where dataCons :: [HsConDecl]
                  dataCons = map f $ M.keys namesMap

                  f :: String -> HsConDecl
                  f eventTyName = mkCon eventTyName [HsUnBangedTy $ mkTyCon eventTyName]

        requestExports :: [HsExportSpec]
        requestExports = [mkExportAll tyName
                         -- ,mkExportAbs toCodeFunName
                         -- ,mkExportAbs hasRepFunName
                         ]

    in appMany (map addExport requestExports)
       . appMany (map addDecl [requestTypDec
                              -- ,toCodeFnTyp
                              -- ,toCodeFnDec
                              -- ,hasRepFnTyp
                              -- ,hasRepFnDec
                              ]) 

mkErrorType :: String -- Module name
            -> M.Map ErrorName Int
            -> HsModule
            -> HsModule
mkErrorType _ namesMap | M.null namesMap = id
mkErrorType name namesMap =
    let tyName = name ++ "Error"

        toCodeFunName = "toErrorCode"
        toCodeFnTyp = mkTypeSig toCodeFunName [] (HsTyFun (mkTyCon tyName) (mkTyCon "Int"))
        toCodeFnDec = HsFunBind $ map go $ M.toList namesMap
            where go (typName, code) = 
                      let cons = typName
                          result = mkNumLit $ fromIntegral code
                      in mkConsMatch toCodeFunName cons result

        errorTypDec = mkDataDecl [] tyName [] dataCons []
            where dataCons :: [HsConDecl]
                  dataCons = map f $ M.keys namesMap

                  f :: String -> HsConDecl
                  f eventTyName = mkCon eventTyName [HsUnBangedTy $ mkTyCon eventTyName]

        errorExports :: [HsExportSpec]
        errorExports = [mkExportAll tyName
                       -- ,mkExportAbs toCodeFunName
                       ]

    in appMany (map addExport errorExports)
           -- . addDecl toCodeFnDec
           -- . addDecl toCodeFnTyp
           . addDecl errorTypDec

appMany :: [a -> a] -> (a -> a)
appMany = foldr (flip (.)) id

