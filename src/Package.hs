{-# LANGUAGE OverloadedStrings #-}
module Package (package, allPackagesSplice) where

import           Data.Maybe
import           Control.Monad

import           Control.Applicative ((<$>))
import           Control.Monad.Trans (MonadIO)

import           Snap.Types (getParam)
import           Snap.Extension.Heist (heistLocal, render)
import           Text.Templating.Heist (Splice, bindString, bindSplice)

import           Data.ByteString.UTF8 (toString)
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Hexpat (renderHtml)

import           Application
import           Database


-- | Handler for a package entry
package :: Application ()
package = do
    packageName <- decodedParam "name"
    heistLocal (bindSplice "dependencies" (dependenciesSplice $ toString packageName) . bindString "name" packageName) $ render "package"
  where
    decodedParam p = fromMaybe "" <$> getParam p


-- | Splice that shows a list of all packages
allPackagesSplice :: (MonadIO m) => Splice m
allPackagesSplice = packageList `liftM` getAllPackages


-- | Splice that shows dependencies for package with given name
dependenciesSplice :: (MonadIO m) => String -> Splice m
dependenciesSplice packageName = do
  dependencies <- getDependencies packageName
  return $ packageList dependencies


-- | Create unorderd list from given list of packages
packageList packages = renderHtml $ H.ul $ mapM_ (H.li . packageLink) packages


-- | A hyperlink to package with given name
packageLink :: String -> Html
packageLink packageName = H.a ! A.href (H.stringValue $ "/package/" ++ packageName) $ H.string packageName
