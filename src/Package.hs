{-# LANGUAGE OverloadedStrings #-}
module Package (package, allPackagesSplice) where

import           Data.Maybe
import           Control.Monad

import           Control.Applicative ((<$>))
import           Control.Monad.Trans (MonadIO)

import           Snap.Types (getParam)
import           Snap.Extension.Heist (heistLocal, render)
import           Text.Templating.Heist (Splice, bindString, bindSplices)

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
    let packageName_ = toString packageName
    let splices = [ ("dependencies", dependenciesSplice packageName_)
                  , ("dependent-packages", dependentPackagesSplice packageName_)
                  ]
    heistLocal ((bindSplices splices) . bindString "name" packageName) $ render "package"
  where
    decodedParam p = fromMaybe "" <$> getParam p


-- | Splice that shows a list of all packages
allPackagesSplice :: (MonadIO m) => Splice m
allPackagesSplice = (renderHtml. packageList) `liftM` getAllPackages


-- | Splice that shows dependencies for package with given name
dependenciesSplice :: (MonadIO m) => String -> Splice m
dependenciesSplice packageName = do
  dependencies <- getDependencies packageName
  return $ renderHtml $ do
    H.h2 ("Dependencies " >> listCountInBraces dependencies)
    if null dependencies
      then
        H.p $ "Package " >> htmlPackageName packageName >> " has no dependencies."
      else
        packageList dependencies


-- | Splice that shows all packages that depend on package with given name
dependentPackagesSplice :: (MonadIO m) => String -> Splice m
dependentPackagesSplice packageName = do
  dependentPackages <- getDependentPackages packageName
  return $ renderHtml $ do
    H.h2 ("Dependent packages " >> listCountInBraces dependentPackages)
    if null dependentPackages
      then
        H.p $ "No other packages depend on " >> htmlPackageName packageName >> "."
      else
        packageList dependentPackages


listCountInBraces :: [a] -> Html
listCountInBraces l = "(" >> H.string (show $ length l) >> ")"


htmlPackageName :: String -> Html
htmlPackageName packageName = H.strong $ H.string packageName


-- | Create unorderd list from given list of packages
packageList :: [String] -> Html
packageList packages = H.ul $ mapM_ (H.li . packageLink) packages


-- | A hyperlink to package with given name
packageLink :: String -> Html
packageLink packageName = H.a ! A.href (H.stringValue $ "/package/" ++ packageName) $ H.string packageName
