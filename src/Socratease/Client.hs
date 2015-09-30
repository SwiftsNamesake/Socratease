-- |
-- Module      : Socratease.Client
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 23 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Socratease.Client where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf
import Control.Monad
import Control.Concurrent

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Socratease.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
start :: IO ()
start = do
  manager <- newManager defaultManagerSettings
  request  <- parseUrl "http://192.168.1.88:8000/flic.html"

  forM [(1::Int)..5] $ \n -> do
    printf "\n\n---- %s %d %s\n\n" "Request" n (replicate 60 '-')
    response <- httpLbs request manager

    printf "The status code was '%s'.\n" (show . statusCode $ responseStatus response)
    printf "The response body was '%s'.\n" (C.unpack $ responseBody response)
    threadDelay . round $ 2.5 * 10^6

  return ()
