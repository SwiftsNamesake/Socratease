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
import           Data.Word

import Text.Printf

import System.Directory

import Control.Applicative
import Control.Monad (void, forM, unless)
import Control.Lens hiding ((.=))

import Database.HDBC
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
instance (FromJSON string, FromJSON auth) => FromJSON (BlogEntry string auth) where
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
instance (IsString string, Convertible SqlValue string) => Convertible [SqlValue] (BlogEntry string Integer) where
  safeConvert [SqlInteger ownid, SqlUTCTime time, SqlString title', SqlString contents', SqlInteger iauthor] = Right $ BlogEntry { _BlogEntryIdof = ownid,
                                                                                                                                   _timestamp     = time,
                                                                                                                                   _title         = fromString title',
                                                                                                                                   _contents      = fromString contents',
                                                                                                                                   _author        = iauthor }
  safeConvert _ = Left $ ConvertError { convSourceValue  = "",
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
entrySchema = "id INTEGER PRIMARY KEY, timestamp DATETIME, contents TEXT, title TEXT, author INTEGER NOT NULL"

authorSchema :: String
authorSchema = "id INTEGER PRIMARY KEY, fullname TEXT NOT NULL"

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


-- |
-- TODO: Move
-- TODO: Dependency injection for testing
-- TODO: Don't re-create if already exists (?)
createBlogDatabase :: IO ()
createBlogDatabase = do
  flip unless (writeFile blogpath "") <$> doesFileExist blogpath
  conn <- connectToBlogDB
  withTransaction conn $ forM [createEntriesTable, createAuthorsTable] . (&)
  return ()

-- Queries ---------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Expand
-- TODO: Include author or just ID (?)
-- TODO: Error handling
-- TODO: Factor out raw query logic
-- TODO: This is unsafe (SQL injections)
selectQuery :: (Convertible [SqlValue] v) => Connection -> String -> String -> String -> IO (Either ConvertError [v])
selectQuery conn table schema whereclause = withTransaction conn $ \conn -> do
  query <- prepare conn $ printf "SELECT (%s) FROM %s WHERE %s" schema table whereclause
  rows <- fetchAllRows query
  return $ mapM safeConvert rows


-- |
queryEntries :: (Convertible SqlValue string, IsString string) => Connection -> String -> IO (Either ConvertError [BlogEntry string Integer])
queryEntries conn = selectQuery conn "entries" entrySchema


-- |
queryAuthors :: (Convertible SqlValue string, IsString string) => Connection -> String -> IO (Either ConvertError [Author string])
queryAuthors conn = selectQuery conn "authors" authorSchema

-- Posts -----------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Error handling
saveEntries :: (Convertible string SqlValue, IsString string) => Connection -> [BlogEntry string Integer] -> IO ()
saveEntries conn entries = do
  insert <- prepare conn $ printf "INSERT INTO %s VALUES (?, ?, ?, ?, ?)" ("entries" :: String)
  either
    (const $ return ())
    (executeMany insert)
    (mapM safeConvert entries)
