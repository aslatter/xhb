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
newXhbTypesModule = addImports . mkModule . typesModuleName


    where addStandardImports = appMany $ map (addImport . mkImport)
              ["Data.Word"
              ,"Foreign.C.Types"
              ,"Data.Bits"
              ,"Data.Binary.Put"
              ,"Data.Binary.Get"
              ,"Data.Typeable"
              ,"Control.Monad"
              ,"Control.Exception"
              ,"Data.List"
              ]
          addHidingImports = addImport $
             mkHidingImport (packagePrefix ++ ".Shared")
                                ["Event"
                                ,"Error"
                                 ]

          addQualImports = addImport $
             mkQualImport (packagePrefix ++ ".Shared")

          addImports = appMany [addStandardImports
                               ,addHidingImports
                               ,addQualImports
                               ]

appMany :: [a -> a] -> (a -> a)
appMany = foldr (flip (.)) id
