-- |
-- Module      : Socratease.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 25 2015

-- TODO | - Separate database types from logic types (or would that incur too much admin overhead) (?)
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Socratease.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS

import Data.Time
import Data.Word



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Rich text, markdown, etc. (for contents)
-- TODO: Polymorphic
-- TODO: Make author type polymorphic (so that we could either fill it with an ID or an author value according to our needs) (?)
-- TODO: Category, comments, keywords, other metadata, etc.
data BlogEntry string auth = BlogEntry { _BlogEntryIdof :: Integer, _timestamp :: UTCTime, _title :: string, _contents :: string, _author :: auth } deriving (Show)


-- |
-- TODO: Extend
-- TODO: Make sure IDs play well with the database
data Author string = Author { _AuthorIdof :: Integer, _fullname :: string } deriving (Show)


-- |
-- newtype IDSQL = IDSQL Word64


-- |
-- data Schema = Schema


-- | Typeclass for types with IDs referring to themselves (as opposed to a related value)
-- class HasOwnID row where
--   getOwnID :: row -> Word64
--   setOwnID :: row -> Word64 -> row
