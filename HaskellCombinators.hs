module HaskellCombinators
{-
    (module Language.Haskell.Syntax
    ,module Language.Haskell.Pretty
    ,mkModule
    ,addExport
    ,addImport
    ,addDecl
    ,mkImport
    )
-}
    where

import Language.Haskell.Syntax
import Language.Haskell.Pretty


-- Example of usage

main = putStrLn $ prettyPrint testHsModule

testHsModule = HsModule dummLoc testModName testModExports testModImports testDecls

testModName = Module "Test.Module"

testModExports = Nothing

testModImports = [mkImport "Foreign"
                 ,mkImport "Data.Monoid"
                 ]

testDecls = [mkTypeSig "testfun" [] (HsTyFun (mkTyCon "Int") (mkTyCon "Int"))
            ,mkSimpleFun "testfun" [HsPWildCard] (HsLit $ HsInt 5)
            ]

-- Wrappers around things in Language.Haskell.Syntax
-- L.H.S is designed to be used as a result of parsing.
-- As I'm going in the other direction, I'd like somehting
-- slightly simpler.
--
-- For example, most of the time I'll be using identifiers instead
-- of symbols, and so all of the combinators here accept strings
-- instead of HsNames, and assume them to be for indetifiers.

dummLoc = SrcLoc "dummy location" 0 0

mkImport str = HsImportDecl dummLoc (Module str) False Nothing Nothing

mkTypeSig var context typ = HsTypeSig dummLoc [HsIdent var] (HsQualType context typ)
mkTyCon = HsTyCon . UnQual . HsIdent


mkTypeDecl :: String -> [String] -> HsType -> HsDecl
mkTypeDecl nm args ty = HsTypeDecl dummLoc (HsIdent nm) (map HsIdent args) ty

mkSimpleFun :: String  -- |name
            -> [HsPat] -- |args
            -> HsExp   -- |body
            -> HsDecl
mkSimpleFun name args rhs = HsFunBind
     [HsMatch dummLoc (HsIdent name) args (HsUnGuardedRhs rhs) []]


mkNewtype :: HsContext
          -> String    -- |name
          -> [String]  -- |type args
          -> HsConDecl -- |constructor
          -> [HsQName] -- |deriving clause
          -> HsDecl
mkNewtype ctxt nam args con drv =
    HsNewTypeDecl
     dummLoc
     ctxt
     (HsIdent nam)
     (map (HsIdent) args)
     con
     drv

mkInstDecl :: HsContext -> HsQName -> [HsType] -> [HsDecl] -> HsDecl
mkInstDecl ctxt clss args decls =
    HsInstDecl dummLoc ctxt clss args decls

mkQName :: String -> String -> HsQName
mkQName
 mod nm = Qual (Module mod) (HsIdent nm)

mkUnQName :: String -> HsQName
mkUnQName = UnQual . HsIdent

mkPVar :: String -> HsPat
mkPVar = HsPVar . HsIdent

mkTyVar :: String -> Type
mkTyVar = HsTyVar . HsIdent

mkDataDecl :: HsContext -> String -> [String] -> [HsConDecl] -> [HsQName] -> HsDecl
mkDataDecl ctx nm args cons drv =
    HsDataDecl
     dummLoc
     ctx
     (HsIdent nm)
     (map HsIdent args)
     cons
     drv

-- |A data constructor declaration.
mkCon :: String -> [HsBangType] -> HsConDecl
mkCon nam typs = HsConDecl dummLoc (HsIdent nam) typs

-- |A record constructor declaration.
mkRCon :: String -> [(String,HsBangType)] -> HsConDecl
mkRCon name fields =
 HsRecDecl
  dummLoc
  (HsIdent name)
  (map (\(f,s) -> ([HsIdent f],s)) fields)


-- |A class declaration
mkClass :: HsContext -> String -> [String] -> [HsDecl] -> HsDecl
mkClass ctxt name vars = HsClassDecl dummLoc ctxt (HsIdent name) (map HsIdent vars)

-- For building up modules

-- |Creates a module with the given name with no imports,
-- no exports and no declarations.
mkModule :: String -> HsModule
mkModule name = HsModule dummLoc (Module name) Nothing [] []

-- |Adds an export to a module
addExport :: HsExportSpec -> HsModule -> HsModule
addExport ex (HsModule loc name exs ims decls) =
    let exs' = case exs of
                 Nothing -> Just [ex]
                 Just xs -> Just $ xs ++ [ex]
    in HsModule loc name exs' ims decls

-- |Adds an import to a module
addImport :: HsImportDecl -> HsModule -> HsModule
addImport im (HsModule loc name exs ims decls) =
    HsModule loc name exs (ims ++ [im]) decls

-- |Adds a declaration to a module
addDecl :: HsDecl -> HsModule -> HsModule
addDecl decl (HsModule loc name exs ims decls) =
    HsModule loc name exs ims (decls ++ [decl])
