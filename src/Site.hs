{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Data.Maybe
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist

import           Application
import           Package (package, allPackagesSplice)


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ render "index"


routes =
  [ ("/",              index)
  , ("/package/:name", package)
  ]

splices =
  [ ("start-time",   startTimeSplice)
  , ("current-time", currentTimeSplice)
  , ("all-packages", allPackagesSplice)
  ]

siteName = "HackageOneFive"


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route routes_ <|> fileServe "resources/static"
  where
    routes_ = map addSplicesAndNames routes
    addSplicesAndNames (name, app) = (name, heistLocal (bindString "site-name" siteName . bindSplices splices) app)
