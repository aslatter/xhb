{-# LANGUAGE DeriveDataTypeable
  , ExistentialQuantification
  #-}

import Text.StringTemplate
import Text.StringTemplate.Classes
import Text.StringTemplate.GenericStandard

import Control.Monad

import qualified Data.Map as M
import Data.List

testMod = buildModule "Test"
          [addImport "Types"
          ,addImport "Classes"
          ,addImportRel "Shape"
          ,addExtension "GeneralizedNewtypeDeriving"
          ,newXid "WINDOW"
          ,newXid "ATOM"
          ,addStruct testStruct
          ]

fieldWindow = StructField "window" "WINDOW" Nothing Nothing Nothing
fieldAtomList = StructField "atoms" "ATOM" Nothing (Just "atomsLength") Nothing
fieldAtomsLength = StructField "atomsLength" "CARD16" Nothing Nothing (Just "atoms")
fieldPad n = StructField ("padding_" ++ show n) "notyp" (Just n) Nothing Nothing

testStruct = Struct "SomeStruct"
             [fieldPad 4
             ,fieldAtomsLength
             ,fieldAtomList
             ,fieldWindow
             ]

main = do
  grp <- (`mergeSTGroups` nullGroup) `liftM` directoryGroup "templates"
  let Just st = getStringTemplate "Proto" grp
  putStrLn $ toString $ setAttribute "basename" "Test.Base" $ withContext st testMod


newXid :: String -> Module -> Module
newXid name = {- ensureUpper -} addXidSyn name . addExport name

-- 

-- I'm way too lazy to come up with a proper
-- type heirarchy.  Existentials to the rescue!
--
-- I can nest these modules as deep as I want,
-- and StringTemplate will know what to do with it.
type Module = M.Map String [Element]
data Element = forall a . (ToSElem a) => Elem a

instance ToSElem Element where
    toSElem (Elem a) = toSElem a

mkModule :: String -> Module
mkModule name = addElem "moduleName" (Elem name) $ M.empty

addElem :: String -> Element -> Module -> Module
addElem nam elem = M.insertWith' (\[x] xs -> x : xs) nam [elem]

addImport :: String -> Module -> Module
addImport = addElem "imports" . Elem

addImportRel :: String -> Module -> Module
addImportRel = addElem "relImports" . Elem

addExport :: String -> Module -> Module
addExport = addElem "exports" . Elem

addExtension :: String -> Module -> Module
addExtension = addElem "extensions" . Elem

addXidSyn :: String -> Module -> Module
addXidSyn = addElem "xidDecls" . Elem

addStruct :: Struct -> Module -> Module
addStruct = addElem "structs" . Elem

buildModule :: String -> [Module -> Module] -> Module
buildModule name opts = foldr (flip (.)) id opts $ mkModule name

-- The "Struct" type

data Struct = Struct {struct_name :: String
                     ,struct_fields :: [StructField]
                     }

data StructField = StructField
    {sfield_name    :: String
    ,sfield_type    :: String
    ,sfield_pad     :: Maybe Int
    ,sfield_list    :: Maybe String  --redundant maybe
    ,sfield_listRef :: Maybe String  --redundant maybe
    }


instance ToSElem Struct where
    toSElem (Struct name fields)
        = SM $
          M.insert "name" (toSElem name) $
          M.insert "fields" (toSElem fields) $
          M.empty

instance ToSElem StructField where
    toSElem (StructField n t p l lr)
     = SM $
       M.insert "name" (toSElem n) $
       M.insert "type" (toSElem t) $
       M.insert "pad"  (toSElem p) $
       M.insert "list" (toSElem l) $
       M.insert "listRef" (toSElem lr) $
       M.empty