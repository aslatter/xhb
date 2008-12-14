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
import Language.Haskell.Syntax

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
isExtension :: Generate Bool
isExtension = 
    (not . isNothing . xheader_xname) <$> current

lookupModule :: MonadReader ReaderData m =>
                String -> m (Maybe XHeader)
lookupModule name = L.find p `liftM` allModules
 where
   p xhd = xheader_header xhd == name

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
     Just xhd ->
         case xheader_name xhd of
           Nothing -> return $ ensureUpper str
           Just fname -> return fname

toHsType :: MonadReader ReaderData m => Type -> m HsType
toHsType (UnQualType name) = return $ mkTyCon $ mapTyNames name
toHsType (QualType qual name) = do
  qname <- fancyName qual
  return $ HsTyCon $ mkQName (typesModuleName qname) name

-- | Some types in the X modules are given using C types.
-- This function maps those strings to the appropriate Haskell
-- types.
mapTyNames :: String -> String
mapTyNames "char" = "CChar"
mapTyNames "void" = "Word8"
mapTyNames "float" = "CFloat"
mapTyNames "double" = "CDouble"
mapTyNames x = x

