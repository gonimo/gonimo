{-# LANGUAGE TypeFamilyDependencies #-}
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
               | DidRead (Identifier r) (DidReadData r)
               | Updated (Identifier r) (UpdatedData r)
               | Deleted (Identifier r)
               deriving Generic


-- | Types a resource must define.
class IsResource r where
  type Identifier r = result | result -> r

  -- | You can override this in order to have a different identifier for reads as for updates/deletes.
  type ReadIdentifier r :: *

  -- | By default this is just the normal 'Identifier'
  type instance ReadIdentifier r = Identifier r

  -- | Data passed to create.
  type CreateData r :: *

  -- | Additonal data returned by 'Created'.
  type CreatedData r :: *

  -- | Data to be passed to 'Update'.
  type UpdateData r :: *

  -- | Data returned by 'Updated'
  type UpdatedData r :: *

  -- | Data passed to 'Read'.
  type ReadData r :: *

  -- | Data retrieved by DidRead. Most likely the resource or parts of it.
  type DidReadData r :: *


-- | Mapping from resources to actual read requests which can be subscribed by clients.
class HasReadRequests resource readRequest where
  getReadRequests :: resource -> [readRequest]
