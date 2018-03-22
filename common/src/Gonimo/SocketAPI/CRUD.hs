{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Gonimo.SocketAPI.CRUD
Description : Common CRUD interface for resources.
Copyright   : (c) Robert Klotzner, 2018

Resources can be Created Read Updated and Deleted, this module provides a common
interface for resources implementing those operations.
-}
module Gonimo.SocketAPI.CRUD ( -- * Types
                               ReqCRUD(..)
                             , ResCRUD(..)
                             , IsResource(..)
                             , HasReadRequests(..)
                             ) where


import GHC.Generics (Generic)


data ReqCRUD r = Create (CreateData r)
               | Read (ReadIdentifier r) (ReadData r)
               | Update (Identifier r) (UpdateData r)
               | Delete (Identifier r) deriving Generic



data ResCRUD r = Created (Identifier r) (CreatedData r)
               | DidRead (ReadIdentifier r) (DidReadData r)
               | Updated (Identifier r) (UpdatedData r)
               | Deleted (Identifier r)
               deriving Generic


-- | Types a resource must define.
class IsResource r where
  type Identifier r :: *

  -- | You can override this in order to have a different identifier for reads as for updates/deletes.
  type ReadIdentifier r :: *
  -- | By default this is just the normal 'Identifier'
  type instance ReadIdentifier r = Identifier r

  type CreateData r :: *
  type CreatedData r :: *

  type UpdateData r :: *
  type UpdatedData r :: *

  type ReadData r :: *
  type DidReadData r :: *


-- | Mapping from resources to actual read requests which can be subscribed by clients.
class HasReadRequests resource readRequest where
  getReadRequests :: resource -> [readRequest]
