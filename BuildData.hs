module BuildData
    (BuildData
    ,BuildResult(..)
    ,EventName
    ,RequestName
    ,ErrorName
    ,applyBuildData
    ,buildHsModule
    ,buildEvent
    ,buildRequest
    ,buildError
    ) where

import Language.Haskell.Syntax
import Data.Monoid
import Data.Map

-- |The state that we build up to eventually build a haskell module
data BuildData = BuildData
    (Endo HsModule) -- |A partial module
    (Map EventName Int) -- |A map from events to event-opcodes
    (Map RequestName (Int, Bool)) -- |A map from requests to (opcodes, has-reply flag)
    (Map ErrorName Int) -- |A map from errors to numbers

type EventName   = String
type RequestName = String
type ErrorName   = String

instance Monoid BuildData where
    mempty = BuildData mempty mempty mempty mempty
    (BuildData a b c d) `mappend` (BuildData a' b' c' d')
        = BuildData
          (a `mappend` a')
          (b `mappend` b')
          (c `mappend` c')
          (d `mappend` d')

applyBuildData :: BuildData -> HsModule -> BuildResult
applyBuildData (BuildData
                (Endo fmod)
                eventMap
                reqMap
                errMap) mod
    = BuildResult
       (fmod mod)
       eventMap
       reqMap
       errMap

-- |The completed HsModule, along with the other data we'll want to use to
-- further add to the module.
data BuildResult =
    BuildResult
      HsModule
      (Map EventName Int)
      (Map RequestName (Int,Bool))
      (Map ErrorName Int)

buildHsModule :: (HsModule -> HsModule) -> BuildData
buildHsModule fmod = BuildData (Endo fmod) mempty mempty mempty

-- |In addition to declaring event types in the module, we also want to store all of the
-- events we've created, as well as the event codes.
buildEvent :: EventName -> Int -> BuildData
buildEvent nm code = BuildData mempty (singleton nm code) mempty mempty

-- |Similar to buildEvent, but for requests.
buildRequest :: RequestName -> Int -> Bool -> BuildData
buildRequest nm code hasReply = BuildData mempty mempty (singleton nm (code, hasReply)) mempty

-- |Similar to buildEvent, but for errors.
buildError :: ErrorName -> Int -> BuildData
buildError nm code = BuildData mempty mempty mempty (singleton nm code)
