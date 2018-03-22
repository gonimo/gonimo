{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module      : Gonimo.Server.CRUD
Description : Common CRUD interface for resources.
Copyright   : (c) Robert Klotzner, 2018

Resources can be Created Read Updated and Deleted, this module provides a common
interface for resource handling on the server.
-}
module Gonimo.Server.CRUD ( -- * Types & Classes
                            IsResourceHandler (..)
                            -- * Handle CRUD requests
                          , handleCRUDRequest
                            -- * Re-exports:
                          , module Gonimo.SocketAPI.CRUD
                          ) where




import           Gonimo.SocketAPI.CRUD


-- | Common infrastructure for handling requests to a resource.
class (IsResource r, Functor m) => IsResourceHandler r m where
  -- | Handle a create request.
  --
  --   Takes some to be defined data, creates the resource and returns an
  --   identifier for the created resource and some optional additonal data.
  handleCreate :: CreateData r -> m (Identifier r, CreatedData r)

  -- | Handle a read.
  --
  --   In some cases one wants to access a resource by some additonal
  --   identifiers than the database id, usually some property contained in the
  --   resource itself: You can make 'ReadIdentifier' a sum type for this. Note
  --   however, 'handleRead' returns an 'Identifier' not a 'ReadIdentifer',
  --   because other identifiers than the database id are contained in the
  --   actual result.
  handleRead :: ReadIdentifier r -> ReadData r -> m (Identifier r, DidReadData r)

  -- | Handle an update.
  handleUpdate :: Identifier r -> UpdateData r -> m (UpdatedData r)

  -- | Handle a delete, no return value expected.
  handleDelete :: Identifier r -> m ()


-- | Handle a generic CRUD request.
handleCRUDRequest :: (IsResourceHandler r m) => ReqCRUD r -> m (ResCRUD r)
handleCRUDRequest req =
  case req of
    Create fid           -> uncurry Created <$> handleCreate fid
    Read readId readData -> uncurry DidRead <$> handleRead readId readData
    Update updId updData -> Updated updId <$> handleUpdate updId updData
    Delete delId         -> Deleted delId <$ handleDelete delId
