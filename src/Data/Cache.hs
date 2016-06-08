module Data.Cache (
    Cache(..)
  , copyCache
  , delete
  , insert
  , insert'
  , lookup
  , lookup'
  , newCache
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

data Cache k v = Cache {
    container :: TVar (HM.HashMap k (CacheItem v))
  , defaultExpiration :: Maybe TimeSpec
}

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

newCache :: Maybe TimeSpec -> IO (Cache k v)
newCache = atomically . newCacheSTM

copyCacheSTM :: Cache k v -> STM (Cache k v)
copyCacheSTM c = do
    m <- newTVar =<< readTVar (container c)
    return c { container = m }

copyCache :: Cache k v -> IO (Cache k v)
copyCache = atomically . copyCacheSTM

sizeSTM :: Cache k v -> STM Int
sizeSTM c = HM.size <$> readTVar (container c)

size :: Cache k v -> IO Int
size = atomically . sizeSTM

deleteSTM :: (Eq k, Hashable k) => k -> Cache k v -> STM ()
deleteSTM k c = writeTVar v =<< (HM.delete k <$> readTVar v) where v = container c

delete :: (Eq k, Hashable k) => k -> Cache k v -> IO ()
delete k c = atomically $ deleteSTM k c

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

lookup' :: (Eq k, Hashable k) => k -> Cache k v -> IO (Maybe v)
lookup' k c = runMaybeT $ item <$> MaybeT (lookupItem False k c)

lookup :: (Eq k, Hashable k) => k -> Cache k v -> IO (Maybe v)
lookup k c = runMaybeT $ item <$> MaybeT (lookupItem True k c)

insertItem :: (Eq k, Hashable k) => k -> CacheItem v -> Cache k v -> STM ()
insertItem k a c = writeTVar v =<< (HM.insert k a <$> readTVar v) where v = container c

insertT :: (Eq k, Hashable k) => k -> v -> Cache k v -> Maybe TimeSpec -> STM ()
insertT k a c t = insertItem k (CacheItem a t) c

insert' :: (Eq k, Hashable k) => k -> v -> Maybe TimeSpec -> Cache k v -> IO ()
insert' k a Nothing c  = atomically $ insertT k a c Nothing
insert' k a (Just d) c = atomically . insertT k a c =<< Just . (d +) <$> now

insert :: (Eq k, Hashable k) => k -> v -> Cache k v -> IO ()
insert k a c = insert' k a (defaultExpiration c) c

now :: IO TimeSpec
now = getTime Monotonic
