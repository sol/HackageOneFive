{-# LANGUAGE OverloadedStrings #-}
module Package (package, splices) where

import           Data.Maybe
import           Control.Applicative ((<$>))
import           Control.Monad.Trans (MonadIO)

import           Snap.Types (getParam)
import           Snap.Extension.Heist (heistLocal, render)
import           Text.Templating.Heist (Splice, bindString)

import           Data.Text.Encoding (decodeUtf8)
import           Data.Text (Text)
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.XmlHtml (renderHtml_)

import           Application
import           Database

import Util (getParamNodeText)

-- | Handler for a package entry
package :: Application ()
package = do
    packageName <- decodedParam "name"
    heistLocal (bindString "name" (decodeUtf8 packageName)) $ render "package"
  where
    decodedParam p = fromMaybe "" <$> getParam p

splices :: (MonadIO m) => [(Text, Splice m)]
splices =
  [ ("dependencies", dependenciesSplice)
  , ("dependent-packages", dependentPackagesSplice)
  , ("all-packages", allPackagesSplice)
  ]

-- | Splice that shows a list of all packages
allPackagesSplice :: (MonadIO m) => Splice m
allPackagesSplice = do
  packages <- getAllPackages
  return $ renderHtml_ $ do
    H.h2 ("All packages " >> listCountInBraces packages)
    packageList packages


-- | Splice that shows dependencies for package with given name
dependenciesSplice :: (MonadIO m) => Splice m
dependenciesSplice = do
  packageName <- getParamNodeText
  dependencies <- getDependencies packageName
  return $ renderHtml_ $ do
    H.h2 ("Dependencies " >> listCountInBraces dependencies)
    if null dependencies
      then
        H.p $ "Package " >> htmlPackageName packageName >> " has no dependencies."
      else
        packageList dependencies


-- | Splice that shows all packages that depend on package with given name
dependentPackagesSplice :: (MonadIO m) => Splice m
dependentPackagesSplice = do
  packageName <- getParamNodeText
  dependentPackages <- getDependentPackages packageName
  return $ renderHtml_ $ do
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
