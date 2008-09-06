{-# LANGUAGE FlexibleContexts #-}

module Generate.Build where

import Generate.Monad
import Data.XCB
import HaskellCombinators

import qualified Data.Map as M
import Language.Haskell.Syntax

import Control.Monad.Reader

import Generate.Facts

--

runGenerate :: ReaderData -> Generate a -> a
runGenerate r m = runReader m r

newXhbTypesModule :: String -> HsModule
newXhbTypesModule = addStandardImports . mkModule . typesModuleName
    where addStandardImports = appMany $ map (addImport . mkImport)
              ["XHB.Shared"
              ,"Data.Word"
              ,"Foreign.C.Types"
              ,"Data.Bits"
              ,"Data.Binary.Put"
              ,"Data.Binary.Get"
              ,"Data.Typeable"
              ,"Control.Monad"
              ,"Control.Exception"
              ,"Data.List"
              ]

appMany :: [a -> a] -> (a -> a)
appMany = foldr (flip (.)) id
