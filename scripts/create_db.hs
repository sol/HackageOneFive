module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import Data.Function (on)

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Package
import Distribution.Text

import Database.HDBC.PostgreSQL (withPostgreSQL, Connection)
import Database.HDBC (run, runRaw, commit, toSql)

import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar (Entry(..), Entries(..), entryPath, EntryContent(..))
import Codec.Compression.GZip (decompress)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B


-- |
-- Read package descriptions from .cabal files (contained in an .tar.gz
-- archive) and insert them into database 'hackage_one_five'.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [archiveName] -> do
      packages <- readArchive archiveName
      withPostgreSQL "dbname = hackage_one_five" $ \conn -> do
        runRaw conn "ALTER TABLE package_dependency DROP CONSTRAINT package_dependency_package_name_fkey"
        runRaw conn "ALTER TABLE package_dependency DROP CONSTRAINT package_dependency_dependency_name_fkey"
        runRaw conn "TRUNCATE package"
        runRaw conn "TRUNCATE package_dependency"
        mapM_ (insertPackage conn) (map latestVersion $ groupByPackageName packages)
        runRaw conn "INSERT INTO package SELECT DISTINCT dependency_name AS name FROM package_dependency WHERE dependency_name NOT IN (SELECT name FROM package)"
        runRaw conn "ALTER TABLE package_dependency ADD FOREIGN KEY (package_name) REFERENCES package"
        runRaw conn "ALTER TABLE package_dependency ADD FOREIGN KEY (dependency_name) REFERENCES package"
        commit conn
    _ -> do
      progName <- getProgName
      putStrLn $ "usage: " ++ progName ++ " <archive.tar.gz>"
      exitFailure
  where
    groupByPackageName = groupBy $ on (==) packageName
    latestVersion = foldl1 packageMax
      where
        packageMax p1 p2 = if packageVersion p1 > packageVersion p2 then p1 else p2


-- | Read package descriptions from an .tar.gz archive
readArchive :: FilePath -> IO [GenericPackageDescription]
readArchive archiveName = do
  archive <- B.readFile archiveName
  processEntries $ Tar.read $ decompress archive
  where
    processEntries :: Entries -> IO [GenericPackageDescription]
    processEntries Done = return []
    processEntries (Fail s) = fail s
    processEntries (Next e es) = do
      case entryContent e of
        NormalFile fileContent _ ->
          if (isSuffixOf ".cabal" filename)
            then
              case parsePackageDescription $ toString fileContent of
                ParseFailed err    -> fail $ show err
                ParseOk warnings p -> do
                  mapM_ print warnings
                  ps <- processEntries es
                  return $ p : ps
            else
              skip
        _                        ->
          skip
      where
        filename = entryPath e
        skip     = do
          putStrLn $ "skipping " ++ filename
          processEntries es


-- | Insert given package description into database
insertPackage :: Connection -> GenericPackageDescription -> IO ()
insertPackage conn p = do
  insertPackage_ conn name dependencies
  where
    name = display $ packageName p
    dependencies = nub $ map (display . \(Dependency name_ _) -> name_) dependencies_
      where
        dependencies_ = packageDeps ++ libDeps ++ execDeps
        packageDeps   = buildDepends (packageDescription p)
        libDeps       = fromMaybe [] $ fmap condTreeConstraints $ condLibrary p
        execDeps      = join $ map (condTreeConstraints . snd) $ condExecutables p


-- | Helper function for 'insertPackage'
insertPackage_ :: Connection -> String -> [String] -> IO ()
insertPackage_ conn name dependencies = do
  _ <- run conn "INSERT INTO package (name) VALUES (?)" [toSql name]
  mapM_ (\d -> run conn "INSERT INTO package_dependency (package_name, dependency_name) VALUES (?, ?)" [toSql name, toSql d]) dependencies
