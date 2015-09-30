-- |
-- Module      : ServerMain
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 16 2015

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
module Main where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Socratease.Types
import qualified Socratease.Server as Server



--------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
  Server.blog
  putStrLn "Shutting down server..."