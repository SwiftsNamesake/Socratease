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

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Socratease.Server where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Text.Printf

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL

import Control.Concurrent
import Control.Lens


makeLenses ''URL



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Pling
startServer = serverWith config handlerequest
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
