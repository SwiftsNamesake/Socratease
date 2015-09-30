-- |
-- Module      : Socratease.Serialise
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 30 2015

-- TODO | - Use TemplateHaskell so I don't miss any fields (?)
--        - Factor out schemas, functions for checking schemas
--        - Parameters from schema (eg. "Integer, Integer, Text" -> "?, ?, ?")
--        - Constructing queries safely
--        - Hide IDs in API functions (?)
--        - Handle persistent DB connections


-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Socratease.Serialise where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
import           Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:))
import           Data.Convertible
import           Data.String
import           Data.Functor
import           Data.Time -- (defaultTimeLocale)
import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.ByteString.Char8 as B

import Text.Printf

import System.Directory

import Control.Applicative
import Control.Monad (void, forM, unless)
import Control.Lens hiding ((.=))

import Database.HDBC
import Database.HDBC.Locale (defaultTimeLocale)
import Database.HDBC.Sqlite3

import Socratease.Types
import Socratease.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------------------------------------------------------------------

-- Save ------------------------------------------------------------------------------------------------------------------------------------

-- |
instance (ToJSON string, IsString string) => ToJSON (Author string) where
  toJSON theauthor = JSON.object ["id"       .= toJSON (theauthor^.authorIdof),
                                  "fullname" .= toJSON (theauthor^.fullname)]


-- |
instance (ToJSON string, ToJSON auth, IsString string) => ToJSON (BlogEntry string auth) where
  toJSON entry = JSON.object ["id"        .= toJSON (entry^.blogEntryIdof),
                              "timestamp" .= toJSON (entry^.timestamp),
                              "title"     .= toJSON (entry^.title),
                              "contents"  .= toJSON (entry^.contents),
                              "author"    .= toJSON (entry^.author)]


-- |
-- TODO: This is quite fragile (cf. schemas)
instance (IsString string, ToJSON string, Convertible string SqlValue) => Convertible (Author string) [SqlValue] where
  safeConvert theauthor = Right [toSql (theauthor^.authorIdof), toSql (theauthor^.fullname)]


-- |
-- TODO: This is quite fragile (cf. schemas)
instance (IsString string, Convertible auth SqlValue, Convertible string SqlValue) => Convertible (BlogEntry string auth) [SqlValue] where
  safeConvert entry = Right [toSql (entry^.blogEntryIdof), toSql (entry^.timestamp), toSql (entry^.title), toSql (entry^.contents), toSql (entry^.author)]

-- Load ------------------------------------------------------------------------------------------------------------------------------------

-- |
instance (FromJSON string) => FromJSON (Author string) where
  parseJSON (JSON.Object o) = Author <$> (o .: "id") <*> (o .: "fullname")


-- |
-- instance (FromJSON string, FromJSON auth) => FromJSON (BlogEntry string auth) where
instance (FromJSON string) => FromJSON (BlogEntry string Integer) where
  parseJSON (JSON.Object o) = BlogEntry <$>
                                (o .: "id")        <*>
                                (o .: "timestamp") <*>
                                (o .: "title")     <*>
                                (o .: "contents")  <*>
                                (o .: "author")


-- |
-- TODO: This is quite fragile (cf. schemas)
instance (IsString string, Convertible SqlValue string) => Convertible [SqlValue] (Author string) where
  safeConvert [SqlInteger ownid, SqlString fullname'] = Right $ Author { _AuthorIdof = ownid, _fullname = fromString fullname' }
  safeConvert _ = Left $ ConvertError { convSourceValue  = "",
                                        convSourceType   = "[SqlValue]",
                                        convDestType     = "Author string",
                                        convErrorMessage = "Failed to construct Author from SQL row" }


-- |
-- TODO: This is quite fragile (cf. schemas)
--  SqlInt64 1,SqlByteString "1873-03-15 06:26:41",SqlByteString "Dummy content",SqlByteString "Oh Make Something Up",SqlInt64 0
instance (IsString string, Convertible SqlValue string) => Convertible [SqlValue] (BlogEntry string Integer) where
  safeConvert [SqlInt64 ownid, SqlByteString time, SqlByteString title', SqlByteString contents', SqlInt64 iauthor] = Right BlogEntry { _BlogEntryIdof = fromIntegral ownid,
                                                                                                                                        _timestamp     = parseTime' $ B.unpack time,
                                                                                                                                        _title         = fromString $ B.unpack title',
                                                                                                                                        _contents      = fromString $ B.unpack contents',
                                                                                                                                        _author        = fromIntegral iauthor }
    where
      parseTime' ts = let Just t = parseTime defaultTimeLocale "%0Y-%m-%d %H:%M:%S" ts in t -- TODO: Handle Nothing case
  safeConvert row = Left $ ConvertError { convSourceValue  = show row,
                                          convSourceType   = "[SqlValue]",
                                          convDestType     = "BlogEntry string Integer",
                                          convErrorMessage = "Failed to construct BlogEntry from SQL row" }



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- Updating/retrieving/creating authors/users/entries/etc.
-- Implement JSON and HTML APIs

-- Connect ---------------------------------------------------------------------------------------------------------------------------------

-- |
connectToBlogDB :: IO Connection
connectToBlogDB = connectSqlite3 blogpath

blogpath = "assets/database/blog.db"

-- Data ------------------------------------------------------------------------------------------------------------------------------------

-- | Schemas
-- TODO: Type safe schemas
entrySchema :: String
entrySchema = "id INTEGER PRIMARY KEY, timestamp DATETIME, title TEXT, contents TEXT, author INTEGER NOT NULL"

entryParameters :: String
entryParameters = "id, timestamp, title, contents, author"

authorSchema :: String
authorSchema = "id INTEGER PRIMARY KEY, fullname TEXT NOT NULL"

authorParameters :: String
authorParameters = "id, fullname"

-- Create ----------------------------------------------------------------------------------------------------------------------------------

-- | TODO: Validation, type safe schemas
createTable :: Connection -> String -> String -> IO ()
createTable conn name schema = do
  query <- prepare conn $ printf "CREATE TABLE IF NOT EXISTS %s (%s)" name entrySchema
  void $ execute query []


-- |
createEntriesTable :: Connection -> IO ()
createEntriesTable conn = createTable conn "entries" entrySchema


-- |
createAuthorsTable :: Connection -> IO ()
createAuthorsTable conn = createTable conn "authors" authorSchema


-- | Uploads some dummy entries
-- TODO: Safe schemas (again)
populateEntriesTable conn = do
  putStrLn "Populating entries table..."
  insert <- prepare conn "INSERT INTO entries (timestamp, title, contents, author) VALUES (?, ?, ?, ?)"
  void $ either
           (const $ putStrLn "Failed to insert dummy entries.")
           (\rows -> executeMany insert (map (drop 1) rows) >> commit conn)
           (mapM safeConvert entries :: Either ConvertError [[SqlValue]])
  where
    entries = [BlogEntry { _BlogEntryIdof = 0, _timestamp = UTCTime { utctDay=ModifiedJulianDay 5232, utctDayTime=23201 }, _title = "Oh Make Something Up", _contents = "Dummy content", _author = 0 }] :: [BlogEntry String Integer]


-- |
-- TODO: Move
-- TODO: Dependency injection for testing
-- TODO: Don't re-create if already exists (?)
createBlogDatabase :: IO ()
createBlogDatabase = do
  putStrLn "Creating blog database..."
  flip unless (putStrLn "Recreating database" >> writeFile blogpath "") <$> doesFileExist blogpath
  conn <- connectToBlogDB
  withTransaction conn $ \conn -> do
    forM [createEntriesTable, createAuthorsTable] ($ conn)
    populateEntriesTable conn
  return ()

-- Queries ---------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Expand
-- TODO: Include author or just ID (?)
-- TODO: Error handling
-- TODO: Factor out raw query logic
-- TODO: This is unsafe (SQL injections)
selectQuery :: (Convertible [SqlValue] v) => Connection -> String -> String -> String -> IO (Either ConvertError [v])
selectQuery conn table parameters _ = withTransaction conn $ \conn -> do
  -- printf "Running query: %s\n" $ (printf "SELECT %s FROM %s WHERE %s" parameters table whereclause :: String)
  -- query <- prepare conn $ printf "SELECT %s FROM %s WHERE %s" parameters table whereclause
  printf "SELECT %s FROM %s\n" parameters table
  query <- prepare conn $ printf "SELECT %s FROM %s" parameters table
  execute query []
  rows <- fetchAllRows query
  return $ mapM safeConvert rows


-- |
queryEntries :: (Convertible SqlValue string, IsString string) => Connection -> String -> IO (Either ConvertError [BlogEntry string Integer])
queryEntries conn = selectQuery conn "entries" entryParameters


-- |
queryAuthors :: (Convertible SqlValue string, IsString string) => Connection -> String -> IO (Either ConvertError [Author string])
queryAuthors conn = selectQuery conn "authors" authorParameters

-- Posts -----------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Error handling
saveEntries :: (Convertible string SqlValue, IsString string) => Connection -> [BlogEntry string Integer] -> IO ()
saveEntries conn entries = do
  insert <- prepare conn $ printf "INSERT INTO %s VALUES (?, ?, ?, ?, ?)" ("entries" :: String)
  either
    (const $ return ())
    (\rows -> executeMany insert rows >> commit conn)
    (mapM safeConvert entries)
