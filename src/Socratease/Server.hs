-- |
-- Module      : Socratease.Server
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 17 2015

-- TODO | - Move out database logic
--        - GUI and console interfaces (proper Unicode console)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Socratease.Server where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS
import Text.Printf
import Data.Functor
import Data.Convertible

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL

import Database.HDBC
import Database.HDBC.Sqlite3

-- import System.Directory

import qualified Data.Aeson as JSON
import           Data.Aeson (toJSON, fromJSON)

import Control.Concurrent
import Control.Monad (void)
import Control.Lens

import Socratease.Types
import qualified Socratease.Serialise as Serialise



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Pling
start :: IO ()
start = serverWith config handlerequest
  where
    handlerequest sockaddr url request = (printf "Request: %s\n" (show url) :: IO ()) >> return response
    headers  = [Header HdrContentType "text/html", Header HdrContentLength (show $ length htmlbody)]
    response = Response (2,0,0) "Reason goes here" headers htmlbody
    config   = Config stdLogger "192.168.1.88" 8000
    htmlbody = unlines ["<doctype html>",
                        "<html>",
                        "    <head>",
                        "        <link rel='stylesheet' type='text/css' href='style.css'>",
                        "    </head>",
                        "    <body>",
                        "        <h1>Hello Internet!</h1>",
                        "    </body>",
                        "</html>"]

    cssbody  = unlines ["",
                        ""]



-- |
-- TODO: JSON API
blog :: IO ()
blog = do
  Serialise.createBlogDatabase
  conn <- Serialise.connectToBlogDB
  serverWith config (handlerequest conn)
  where
    handlerequest conn sockaddr url request = response conn sockaddr url request
    response conn sockaddr url request = case request of
      (rqMethod -> GET)  -> sendEntries conn request
      (rqMethod -> POST) -> postEntries conn request
      _                      -> return $ Response (5,0,1) "This method is not yet supported." [Header HdrContentType "0"] ""
    config = Config stdLogger "192.168.1.88" 8000


-- |
-- TODO: More advanced API for query strings
-- TODO: JSON API for GET method
sendEntries :: Connection -> Request BS.ByteString -> IO (Response BS.ByteString)
sendEntries conn request = either errorResponse entryresponse <$> (Serialise.queryEntries conn "TRUE")
  where
    entryresponse entries = Response (2,0,0) "Reason goes here" (headers $ payload entries) (payload entries)
    errorResponse _       = Response (4,0,4) "Failed to retrieve entries. Sorry." [Header HdrContentLength "0"] ""
    payload entries = JSON.encode entries
    headers thedata = [Header HdrContentType "text/json", Header HdrContentLength (show $ BS.length thedata)]


-- |
-- TODO: More advanced API for query strings
-- TODO: JSON API for GET method
-- TODO: Error handling
postEntries ::  Connection -> Request BS.ByteString -> IO (Response BS.ByteString)
postEntries conn request = maybe errorResponse successResponse (JSON.decode $ rqBody request)
  where
    successResponse json = Serialise.saveEntries conn json >> return (Response (2,0,0) "Succesfully posted entries" [Header HdrContentLength "0"] "")
    errorResponse        = return $ Response (0,0,0) "Failed to upload entries" [Header HdrContentLength "0"] ""
