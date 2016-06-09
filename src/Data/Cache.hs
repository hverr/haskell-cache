-- |
-- Module:      Data.Cache
-- Copyright:   (c) 2016 Henri Verroken
-- LIcense:     BSD3
-- Maintainer:  Henri Verroken <henriverroken@gmail.com>
-- Stability:   experimental
--
-- An in-memory key/value store with expiration support, similar
-- to patrickmn/go-cache for Go.
--
-- The cache is a shared mutable HashMap implemented using STM. It
-- supports item expiration.

module Data.Cache (
    -- * How to use this library
    -- $use

    -- * Creating a cache
    Cache
  , newCache

    -- * Cache properties
  , defaultExpiration
  , setDefaultExpiration
  , copyCache

    -- * Managing items
    -- ** Insertion
  , insert
  , insert'
    -- ** Querying
  , lookup
  , lookup'
  , keys
    -- ** Deletion
  , delete
  , purgeExpired

    -- * Cache information
  , size
) where

import Prelude hiding (lookup)

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe
import System.Clock

-- | The cache with keys of type @k@ and values of type @v@.
--
-- Create caches with the 'newCache' and 'copyCache' functions.
data Cache k v = Cache {
    container :: TVar (HM.HashMap k (CacheItem v))
    -- | The default expiration value of newly added cache items.
    --
    -- See 'newCache' for more information on the default expiration value.
  , defaultExpiration :: Maybe TimeSpec
}

-- | Change the default expiration value of newly added cache items.
--
-- See 'newCache' for more information on the default expiration value.
setDefaultExpiration :: Cache k v -> Maybe TimeSpec -> Cache k v
setDefaultExpiration c t = c { defaultExpiration = t }

data CacheItem v = CacheItem {
    item :: v
  , itemExpiration :: Maybe TimeSpec
}

isExpired :: TimeSpec -> CacheItem v -> Bool
isExpired t i = fromMaybe False (itemExpiration i >>= f t)
    where f now' e
            | e < now'   = Just True
            | otherwise = Just False

newCacheSTM :: Maybe TimeSpec -> STM (Cache k v)
newCacheSTM d = do
    m <- newTVar HM.empty
    return Cache { container = m, defaultExpiration = d }

-- | Create a new cache with a default expiration value for newly
-- added cache items.
--
-- Items that are added to the cache without an explicit expiration value
-- (using 'insert') will be inserted with the default expiration value.
--
-- If the specified default expiration value is `Nothing`, items inserted
-- by 'insert' will never expire.
newCache :: Maybe TimeSpec -> IO (Cache k v)
newCache = atomically . newCacheSTM

copyCacheSTM :: Cache k v -> STM (Cache k v)
copyCacheSTM c = do
    m <- newTVar =<< readTVar (container c)
    return c { container = m }

-- | Create a deep copy of the cache.
copyCache :: Cache k v -> IO (Cache k v)
copyCache = atomically . copyCacheSTM

sizeSTM :: Cache k v -> STM Int
sizeSTM c = HM.size <$> readTVar (container c)

-- | Return the size of the cache, including expired items.
size :: Cache k v -> IO Int
size = atomically . sizeSTM

deleteSTM :: (Eq k, Hashable k) => k -> Cache k v -> STM ()
deleteSTM k c = writeTVar v =<< (HM.delete k <$> readTVar v) where v = container c

-- | Delete an item from the cache. Won't do anything if the item is not present.
delete :: (Eq k, Hashable k) => Cache k v -> k -> IO ()
delete c k = atomically $ deleteSTM k c

lookupItem' :: (Eq k, Hashable k) => k -> Cache k v -> STM (Maybe (CacheItem v))
lookupItem' k c = HM.lookup k <$> readTVar (container c)

lookupItemT :: (Eq k, Hashable k) => Bool -> k -> Cache k v -> TimeSpec -> STM (Maybe (CacheItem v))
lookupItemT del k c t = runMaybeT $ do
        i <- MaybeT (lookupItem' k c)
        let e = isExpired t i
        _ <- when (e && del) (MaybeT $ Just <$> deleteSTM k c)
        if e then MaybeT $ return Nothing else MaybeT $ return (Just i)

lookupItem :: (Eq k, Hashable k) => Bool -> k -> Cache k v -> IO (Maybe (CacheItem v))
lookupItem del k c = (atomically . lookupItemT del k c) =<< now

-- | Lookup an item with the given key, but don't delete it if it is expired.
--
-- The function will only return a value if it is present in the cache and if
-- the item is not expired.
--
-- The function will not delete the item from the cache.
lookup' :: (Eq k, Hashable k) => Cache k v -> k -> IO (Maybe v)
lookup' c k = runMaybeT $ item <$> MaybeT (lookupItem False k c)

-- | Lookup an item with the given key, and delete it if it is expired.
--
-- The function will only return a value if it is present in the cache and if
-- the item is not expired.
--
-- The function will eagerly delete the item from the cache if it is expired.
lookup :: (Eq k, Hashable k) => Cache k v -> k -> IO (Maybe v)
lookup c k = runMaybeT $ item <$> MaybeT (lookupItem True k c)

insertItem :: (Eq k, Hashable k) => k -> CacheItem v -> Cache k v -> STM ()
insertItem k a c = writeTVar v =<< (HM.insert k a <$> readTVar v) where v = container c

insertT :: (Eq k, Hashable k) => k -> v -> Cache k v -> Maybe TimeSpec -> STM ()
insertT k a c t = insertItem k (CacheItem a t) c

-- | Insert an item in the cache, with an explicit expiration value.
--
-- If the expiration value is 'Nothing', the item will never expire. The
-- default expiration value of the cache is ignored.
insert' :: (Eq k, Hashable k) =>  Cache k v -> Maybe TimeSpec -> k -> v -> IO ()
insert' c Nothing k a  = atomically $ insertT k a c Nothing
insert' c (Just d) k a = atomically . insertT k a c =<< Just . (d +) <$> now

-- | Insert an item in the cache, using the default expiration value of
-- the cache.
insert :: (Eq k, Hashable k) => Cache k v -> k -> v -> IO ()
insert c = insert' c (defaultExpiration c)

keysSTM :: Cache k v -> STM [k]
keysSTM c = HM.keys <$> readTVar (container c)

-- | Return all keys present in the cache.
keys :: Cache k v -> IO [k]
keys = atomically . keysSTM

now :: IO TimeSpec
now = getTime Monotonic

purgeExpiredSTM :: (Eq k, Hashable k) => Cache k v -> TimeSpec -> STM ()
purgeExpiredSTM c t = mapM_ (\k -> lookupItemT True k c t) =<< keysSTM c

-- | Delete all items that are expired.
--
-- This is one big atomic operation.
purgeExpired :: (Eq k, Hashable k) => Cache k v -> IO ()
purgeExpired c = (atomically . purgeExpiredSTM c) =<< now

-- $use
--
-- All operations are atomically executed in the IO monad. The
-- underlying data structure is @Data.HashMap.Strict@.
--
-- First create a cache using 'newCache' and possibly a default
-- expiration value. Items can now be inserted using 'insert' and
-- 'insert''.
--
-- 'lookup' and 'lookup'' are used to query items. These functions
-- only return a value when the item is in the cache and it is not
-- expired. The 'lookup' function will automatically delete the
-- item if it is expired, while 'lookup'' won't delete the item.
--
-- > >>> c <- newCache Nothing :: IO (Cache String String)
-- > >>> insert c "key" "value"
-- > >>> lookup c "key"
-- > Just "value"
-- > >>> delete c "key"
-- > >>> lookup c "key"
-- > Nothing
