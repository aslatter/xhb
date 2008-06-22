-- |Types used during code generation, shared between 'Generate' and
-- Generate.PostProcessing
module Generate.Types where


import Control.Applicative

import Control.Monad.RW

import Data.Monoid

import BuildData
import XCB.Types

{-

output from genertion:

an HsModule

However, as an intermediate result, we need:

 + a partial HsModule (represented by a morphism (HsModule -> HsModule))
 + meta data about the module:
   + Event meta data
   + Errors meta data
   + Request meta data
   + ???

information needed during parsing:

 + Stuff from the header
 + the list of declarations
 + a mapping from all other module names to fancy-names

the list of dclarations is provided as the input.
the other information is provided in a reader module.

since the output data does not need to be read during processing, it will be dumped out using a writer monad

-}

-- during processing

data ReaderData = ReaderData
    {readerData_current :: XHeader
    ,readerData_all :: [XHeader]
    }
type Generate a = RW ReaderData BuildData a
type Gen = Generate ()

-- post processing is a function :: BuildResult -> ReadData -> HsModule

