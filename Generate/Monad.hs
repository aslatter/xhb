{-# LANGUAGE FlexibleContexts #-}

-- |Types used during code generation, shared between 'Generate' and
-- Generate.PostProcessing
module Generate.Monad where

import HaskellCombinators

import Data.XCB.Types

import Generate.Util
import Generate.Facts

import qualified Data.List as L
import Control.Monad.Reader
import Data.Maybe
import Control.Applicative

data ReaderData = ReaderData
    {readerData_current :: XHeader
    ,readerData_all :: [XHeader]
    }

type Generate = Reader ReaderData
-- primitives

current :: MonadReader ReaderData m => m XHeader
current = readerData_current `liftM` ask

allModules :: MonadReader ReaderData m => m [XHeader]
allModules = readerData_all `liftM` ask

-- more advanced functions

-- | are we generating an extension module?
isExtension :: GenXHeader a -> Bool
isExtension = 
    not . isNothing . xheader_xname

lookupModule :: MonadReader ReaderData m =>
                String -> m (Maybe XHeader)
lookupModule name = L.find p `liftM` allModules
 where
   p xhd = xheader_header xhd == name

findModule :: String -> [GenXHeader a] -> Maybe (GenXHeader a)
findModule str xs = L.find p xs
 where
   p xhd = xheader_header xhd == str

currentDeclarations :: Generate [XDecl]
currentDeclarations = xheader_decls <$> current

currentName :: Generate Name
currentName = xheader_header <$> current

-- Intercaps name
fancyName :: MonadReader ReaderData m =>
             Name -> m Name
fancyName str = do
   mod <- lookupModule str
   case mod of
     Nothing -> error $ "Error resolving module name: " ++ str
     Just xhd -> return $ formatName xhd

formatName :: GenXHeader a -> String
formatName xhd
    = ensureUpper $ case xheader_name xhd of
        Nothing  -> xheader_header xhd
        Just str -> str

toHsType :: MonadReader ReaderData m => Type -> m HsType
toHsType (UnQualType name) = return $ mkTyCon $ mapTyNames name
toHsType (QualType qual name) = do
  qname <- fancyName qual
  return $ tyCon $ mkQName (typesModuleName qname) name

resolveTypes :: [XHeader] -> [HXHeader]
resolveTypes xs = map (resolveTypes' xs) xs

resolveTypes' xs x = mapTypes f x
 where f typ = flip runReader r $ toHsType typ
       r = ReaderData x xs

type HXHeader = GenXHeader HsType
type HXDecl = GenXDecl HsType
type HStructElem = GenStructElem HsType
type HXReply = GenXReply HsType

-- | Some types in the X modules are given using C types.
-- This function maps those strings to the appropriate Haskell
-- types.
mapTyNames :: String -> String
-- mappings to equivalent CTypes
mapTyNames "char" = "CChar"
mapTyNames "void" = "Word8"
mapTyNames "float" = "CFloat"
mapTyNames "double" = "CDouble"
-- mappings to roughly equivalent Haskell types
mapTyNames "BOOL" = "Bool"
-- mappings to equivalent Hasekll types
mapTyNames "CARD8" = "Word8"
mapTyNames "CARD16" = "Word16"
mapTyNames "CARD32" = "Word32"
mapTyNames "INT8" = "Int8"
mapTyNames "INT16"= "Int16"
mapTyNames "INT32" = "Int32"
mapTyNames "BYTE" = "Word8"
mapTyNames x = x

