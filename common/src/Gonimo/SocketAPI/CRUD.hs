{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-|
Module      : Gonimo.SocketAPI.CRUD
Description : Common CRUD interface for resources.
Copyright   : (c) Robert Klotzner, 2018

Resources can be Created Read Updated and Deleted, this module provides a common
interface for resources implementing those operations.

Not used!

This CRUD based approach does not really fit for our application, most concerning issue:

Some requests are simply more complex than modifying just one resource.
Sometimes many related resources need to be modified and those modifications
might need to be atomic or are at least much more performant, when carried out
by the server at once, instead of letting the client issue one request after the
other. Example: Leaving a family: An entry in the family_account table gets
removed, but if the account was the last member of the family, the family needs
to be deleted as well.

The other issue: This approach should have helped in having principled
subscriber notifications, but that is not that simple as well: While Update
requests are usually localized to a given resource, create and delete requests
are most often not: They usuall will have an effect on other resources as well,
e.g. deliting an invitation will update the family_invitations resource.
-}
module Gonimo.SocketAPI.CRUD ( -- * Types
                               ReqCRUD(..)
                             , ResCRUD(..)
                             , IsResource(..)
                             , HasReadRequests(..)
                             ) where


import           GHC.Generics (Generic)


data ReqCRUD r = Create (CreateData r)
               | Read (RequestIdentifier r) (ReadData r)
               | Update (RequestIdentifier r) (UpdateData r)
               | Delete (RequestIdentifier r)
               deriving Generic



data ResCRUD r = Created (Identifier r) (CreatedData r)
               | DidRead (Identifier r) (DidReadData r)
               | Updated (Identifier r) (UpdatedData r)
               | Deleted (Identifier r)
               deriving Generic


-- | Types a resource must define.
class IsResource r where
  type Identifier r = result | result -> r

  -- | You can override this in order to have a different identifier for requests.
  type RequestIdentifier r :: *

  -- | By default this is just the normal 'Identifier'
  type instance RequestIdentifier r = Identifier r

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
