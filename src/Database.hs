module Database (getDependencies, getAllPackages) where

import           Control.Monad.Trans (MonadIO, liftIO)

import           Database.HDBC.PostgreSQL (withPostgreSQL, Connection)
import           Database.HDBC


-- | List of dependencies for package with given name
getDependencies :: (MonadIO m) => String -> m [String]
getDependencies packageName = withDB $ \conn -> do
  r <- quickQuery' conn "SELECT dependency_name FROM package_dependency WHERE package_name = ? ORDER BY dependency_name" [toSql packageName]
  return $ map (fromSql . head) r


-- | List of dependencies for package with given name
getAllPackages :: (MonadIO m) => m [String]
getAllPackages = withDB $ \conn -> do
  r <- quickQuery' conn "SELECT name FROM package ORDER BY name" []
  return $ map (fromSql . head) r


withDB :: (MonadIO m) => (Connection -> IO a) -> m a
withDB = liftIO . withPostgreSQL "dbname = hackage_one_five"
