-- |
-- Module:      Data.Cache
-- Copyright:   (c) 2016 Henri Verroken
-- License:     BSD3
-- Maintainer:  Henri Verroken <henriverroken@gmail.com>
-- Stability:   stable
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
  , newCacheSTM

    -- * Cache properties
  , defaultExpiration
  , setDefaultExpiration
  , copyCache
  , copyCacheSTM

    -- * Managing items
    -- ** Insertion
  , insert
  , insert'
  , insertSTM
    -- ** Querying
  , lookup
  , lookup'
  , lookupSTM
  , keys
  , keysSTM
    -- ** Deletion
  , delete
  , deleteSTM
  , filterWith
  , purge
  , purgeExpired
  , purgeExpiredSTM
    -- ** Combined actions
  , fetchWithCache

    -- * Cache information
  , size
  , sizeSTM
) where

import Prelude hiding (lookup)

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Cache.Internal
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe
import System.Clock

-- | Change the default expiration value of newly added cache items.
--
-- See 'newCache' for more information on the default expiration value.
setDefaultExpiration :: Cache k v -> Maybe TimeSpec -> Cache k v
setDefaultExpiration c t = c { defaultExpiration = t }


isExpired :: TimeSpec -> CacheItem v -> Bool
isExpired t i = fromMaybe False (itemExpiration i >>= f t)
    where f now' e
            | e < now'   = Just True
            | otherwise = Just False

-- | Create a new cache with a default expiration value for newly
-- added cache items.
--
-- Items that are added to the cache without an explicit expiration value
-- (using 'insert') will be inserted with the default expiration value.
--
-- If the specified default expiration value is `Nothing`, items inserted
-- by 'insert' will never expire.
newCache :: Maybe TimeSpec -> IO (Cache k v)
newCache d = do
    m <- newTVarIO HM.empty
    return Cache { container = m, defaultExpiration = d }

-- | STM variant of 'newCache'
newCacheSTM :: Maybe TimeSpec -> STM (Cache k v)
newCacheSTM d = do
    m <- newTVar HM.empty
    return Cache { container = m, defaultExpiration = d }

copyCacheSTM :: Cache k v -> STM (Cache k v)
copyCacheSTM c = do
    m <- newTVar =<< readTVar (container c)
    return c { container = m }

-- | Create a deep copy of the cache.
copyCache :: Cache k v -> IO (Cache k v)
copyCache = atomically . copyCacheSTM

-- | STM variant of 'size'
sizeSTM :: Cache k v -> STM Int
sizeSTM c = HM.size <$> readTVar (container c)

-- | Return the size of the cache, including expired items.
size :: Cache k v -> IO Int
size = atomically . sizeSTM

-- | STM variant of 'delete'.
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

-- | Lookup an item with a given key in the 'STM' monad, given the current 'Monotonic' time.
--
-- STM variant of 'lookup' and 'lookup''
lookupSTM :: (Eq k, Hashable k) => Bool             -- ^ Whether or not to eagerly delete the item if its expired
                                -> k                -- ^ The key to lookup
                                -> Cache k v        -- ^ The cache
                                -> TimeSpec         -- ^ The current 'Monotonic' time, i.e. @getTime Monotonic@
                                -> STM (Maybe v)
lookupSTM f k c t = do
    mv <- lookupItemT f k c t
    return $! item <$> mv

insertItem :: (Eq k, Hashable k) => k -> CacheItem v -> Cache k v -> STM ()
insertItem k a c = writeTVar v =<< (HM.insert k a <$> readTVar v) where v = container c

-- | Insert an item in the cache, with an explicit expiration value, in the
-- 'STM' monad.
--
-- If the expiration value is 'Nothing', the item will never expire. The
-- default expiration value of the cache is ignored.
--
-- The expiration value is the absolute 'Monotonic' time the item expires. You
-- should manually construct the absolute 'Monotonic' time, as opposed to the
-- behaviour of 'insert''.
--
-- E.g.
--
-- > action :: Cache -> IO ()
-- > action c = do
-- >     t <- getTime Monotonic
-- >     let t' = t + (defaultExpiration c)
-- >     atomically $ insertSTM 0 0 c (Just t')
--
insertSTM :: (Eq k, Hashable k) => k -> v -> Cache k v -> Maybe TimeSpec -> STM ()
insertSTM k a c t = insertItem k (CacheItem a t) c

-- | Insert an item in the cache, with an explicit expiration value.
--
-- If the expiration value is 'Nothing', the item will never expire. The
-- default expiration value of the cache is ignored.
--
-- The expiration value is relative to the current 'Monotonic' time, i.e. it
-- will be automatically added to the result of @getTime Monotonic@.
insert' :: (Eq k, Hashable k) =>  Cache k v -> Maybe TimeSpec -> k -> v -> IO ()
insert' c Nothing k a  = atomically $ insertSTM k a c Nothing
insert' c (Just d) k a = atomically . insertSTM k a c =<< Just . (d +) <$> now

-- | Insert an item in the cache, using the default expiration value of
-- the cache.
insert :: (Eq k, Hashable k) => Cache k v -> k -> v -> IO ()
insert c = insert' c (defaultExpiration c)

-- | Get a value from cache. If not available from cache, use the provided action and update the cache.
-- Note that the cache check and conditional execution of the action is not one atomic action.
fetchWithCache :: (Eq k, Hashable k, MonadIO m) => Cache k v -> k -> (k -> m v) -> m v
fetchWithCache c k f = do
  mv <- liftIO $ lookup c k
  case mv of
    Just v -> return v
    Nothing -> do
       v <- f k
       liftIO $ insert c k v
       return v

-- | STM variant of 'keys'.
keysSTM :: Cache k v -> STM [k]
keysSTM c = HM.keys <$> readTVar (container c)

-- | Return all keys present in the cache.
keys :: Cache k v -> IO [k]
keys = atomically . keysSTM

now :: IO TimeSpec
now = getTime Monotonic

-- | keeps elements that satify a predicate (used for cache invalidation).
filterWith :: (Eq k, Hashable k) => (k -> Bool) -> Cache k v -> IO ()
filterWith f c = atomically $ writeTVar v =<< (HM.filterWithKey (\k _ -> f k) <$> readTVar v) where v = container c

-- | delete all elements (cache invalidation).
purge :: (Eq k, Hashable k) => Cache k v -> IO ()
purge c = atomically $ writeTVar v HM.empty where v = container c

-- | STM variant of 'purgeExpired'.
--
-- The 'TimeSpec' argument should be the current 'Monotonic' time, i.e.
-- @getTime Monotonic@.
purgeExpiredSTM :: (Eq k, Hashable k) => Cache k v -> TimeSpec -> STM ()
purgeExpiredSTM c t = mapM_ (\k -> lookupItemT True k c t) =<< keysSTM c

-- | Delete all items that are expired.
--
-- This is one big atomic operation.
purgeExpired :: (Eq k, Hashable k) => Cache k v -> IO ()
purgeExpired c = (atomically . purgeExpiredSTM c) =<< now

-- $use
--
-- All operations are automically executed in the IO monad. The
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
-- Note that items are __not purged automatically__ in the background when they
-- expire. You have to manually call 'lookup' to purge a single item, or call
-- 'purgeExpired' to purge all expired items.
--
-- > >>> c <- newCache Nothing :: IO (Cache String String)
-- > >>> insert c "key" "value"
-- > >>> lookup c "key"
-- > Just "value"
-- > >>> delete c "key"
-- > >>> lookup c "key"
-- > Nothing
