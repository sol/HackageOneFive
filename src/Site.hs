{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
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


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = app <|> serveDirectory "resources/static"


-- | Main handler for dynamic content
app :: Application ()
app = heistLocal (bindStrings strings . bindSplices splices) $
  route routes
  where
    routes =
      [ ("/",              index)
      , ("/package/:name", package)
      ]

    splices =
      [ ("start-time",   startTimeSplice)
      , ("current-time", currentTimeSplice)
      , ("all-packages", allPackagesSplice)
      ]

    strings =
      [ ("site-name", "HackageOneFive") ]
