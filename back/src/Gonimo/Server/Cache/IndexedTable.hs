{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Gonimo.Server.Cache.IndexedTable
Description : An indexed table is a map with an additional lookup (the index).
Copyright   : (c) Robert Klotzner, 2017

This module provides indexed tables, where you can lookup values not only by a key, but also by some property of the value - the index. Think of data base indeces.

If you need multiple indeces, you can nest 'IndexedTable', for an example checkout "Gonimo.Server.Cache.FamilyAccounts".
-}
module Gonimo.Server.Cache.IndexedTable (
                                          -- * Types
                                          IndexedTable
                                          -- * Creation & expansion
                                        , fromRawTable
                                        , loadData
                                          -- * Access inner tables and the index
                                        , getInner
                                        , getIndex
                                        , lookupByIndex
                                        , HasRawTable(..)
                                        ) where
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Control.Lens hiding (index)
import Prelude hiding (lookup)




-- | A table with an additonal index which is kept in sync automatically.
data IndexedTable index c key val
  = IndexedTable { _table :: c key val
                 , _byIndex :: Map index (Set key)
                 , _valToIndex :: val -> Maybe index
                 }

-- | Build an indexed table from a raw table.
--
--   A raw table can be a plain map or another IndexedTable, thus an IndexTable can have nested indeces.
fromRawTable :: (Eq key, Ord key, Ord index, HasRawTable t) => (val -> Maybe index) -> t key val -> IndexedTable index t key val
fromRawTable valToIndex' m = IndexedTable { _table = m
                                          , _byIndex = buildIndex valToIndex' (toRawTable m)
                                          , _valToIndex = valToIndex'
                                          }

-- | Convenience function for mass insertion into the table.
loadData :: (At (c key val), Ord key
            , IxValue (c key val) ~ val
            , Index (c key val) ~ key
            , Ord index
            )
         => [(key, val)] -> IndexedTable index c key val -> IndexedTable index c key val
loadData = flip $ foldr (uncurry insert)

-- | Get the inner table, which can be another 'IndexedTable' or a plain 'Map'.
getInner :: IndexedTable index c key val -> c key val
getInner = _table

-- | Search a key by index.
-- TODO: Data.Map is too strict! If you do an fmap over it, the whole tree will
-- be rebuilt when accessing any value. The values are kept lazy, but the whole
-- tree will be built! This means a search by index is in effect a linear
-- search, making the whole indexed map idea non sensical.
-- Solution: Either don't use a Set but lists internaly or just returen a Set or use some data structure like this:
--
-- @@@
--   data LazyMap key val newval
--      = LazyMap { strictMap :: Map key val
--                , modifier :: (val -> newval)
--                }
-- @@@
-- It is a shame that Map is built this way, Haskell's lazyness would be really awesome here.
getIndex :: IndexedTable index c key val -> Map index (Set key)
getIndex = _byIndex


-- | Convenience function, if want to do a quick lookup based on the index.
lookupByIndex :: (At (c key val), Ord key, Ord index, IxValue (c key val) ~ val
                 , Index (c key val) ~ key)
  => index -> IndexedTable index c key val -> [(key, val)]
lookupByIndex index t = fromMaybe [] $ do
  keys <- t ^? byIndex . at index . _Just . to Set.toList
  pure $ mapMaybe (\k -> (k, ) <$> t ^. table . at k) keys


-- | Get access to the underlying table as a Map
class HasRawTable t where
  toRawTable :: t key val -> Map key val

instance HasRawTable Map where
  toRawTable = id

instance HasRawTable c => HasRawTable (IndexedTable index c) where
  toRawTable = toRawTable . _table

insert :: forall c key val index
          . (At (c key val), Ord key
            , IxValue (c key val) ~ val
            , Index (c key val) ~ key
            , Ord index
            )
       => key -> val -> IndexedTable index c key val -> IndexedTable index c key val
insert key val fTable = fTable & table.at key .~ Just val
                               & maybe id insertIndexEntry mFKey
  where
    insertIndexEntry :: index -> IndexedTable index c key val -> IndexedTable index c key val
    insertIndexEntry fKey = byIndex . at fKey . non Set.empty . at key .~ Just ()

    mFKey :: Maybe index
    mFKey = (fTable^.valToIndex) val


delete :: ( At (c key val)
          , IxValue (c key val) ~ val
          , Index (c key val) ~ key
          , Ord key
          , Ord index
          )
       => key -> IndexedTable index c key val -> IndexedTable index c key val
delete key fTable = fTable & table.at key .~ Nothing
                           & maybe id deleteIndexEntry mFKey
  where
    deleteIndexEntry fKey = byIndex . at fKey . non Set.empty . at key .~ Nothing
    mFKey = fTable ^? table . at key . _Just . to (fTable^.valToIndex) . _Just


lookup :: ( At (c key val)
          , IxValue (c key val) ~ val
          , Index (c key val) ~ key
          , Ord key
          ) => key -> IndexedTable index c key val -> Maybe val
lookup key = (^. table . at key)


buildIndex :: forall key val index
  . (Ord index, Eq key, Ord key)
  => (val -> Maybe index) -> Map key val -> Map index (Set key)
buildIndex valToIndex' = foldr addToIndex Map.empty . Map.toList
  where
    addToIndex :: (key, val) -> Map index (Set key) -> Map index (Set key)
    addToIndex (key, val) = fromMaybe id $ do
      index' <- valToIndex' val
      pure $ at index' . non Set.empty . at key .~ Just ()


type instance Index (IndexedTable index c key val) = key
type instance IxValue (IndexedTable index c key val) = val

instance ( Ord key, Ixed (c key val), IxValue (c key val) ~ val
         , Index (c key val) ~ key
         , At (c key val)
         , Ord index
         ) => Ixed (IndexedTable index c key val) where
  ix k f m = case lookup k m of
      Just v  -> f v <&> \v' -> insert k v' m
      Nothing -> pure m

instance ( Ord key, At (c key val)
         , IxValue (c key val) ~ val
         , Index (c key val) ~ key
         , Ord index
         ) => At (IndexedTable index c key val) where
  -- TODO: This can be optimized similar to Map by providing alterF.
  at k f m = f mv <&> \r -> case r of
      Nothing -> maybe m (const (delete k m)) mv
      Just v' -> insert k v' m
      where mv = lookup k m


-- Lenses for IndexedTable index c key val:

-- Lenses for IndexedTable index c key val:

table :: Lens' (IndexedTable index c key val) (c key val)
table f indexedTable' = (\table' -> indexedTable' { _table = table' }) <$> f (_table indexedTable')

byIndex :: Lens' (IndexedTable index c key val) (Map index (Set key))
byIndex f indexedTable' = (\byIndex' -> indexedTable' { _byIndex = byIndex' }) <$> f (_byIndex indexedTable')

valToIndex :: Lens' (IndexedTable index c key val) (val -> Maybe index)
valToIndex f indexedTable' = (\valToIndex' -> indexedTable' { _valToIndex = valToIndex' }) <$> f (_valToIndex indexedTable')


