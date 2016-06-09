module Data.CacheSpec (main, spec) where

import Prelude hiding (lookup)

import Test.Hspec

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Data.Cache
import System.Clock

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should have a deletion/non-deletion variant" $ do
        c <- liftIO $ defCache Nothing
        _ <- liftIO $ expire defExpiration
        liftIO (size c)                  >>= (`shouldBe` 4)
        liftIO (lookup' c (fst expired))>>= (`shouldBe` Nothing)
        liftIO (size c)                  >>= (`shouldBe` 4)
        liftIO (lookup  c (fst expired)) >>= (`shouldBe` Nothing)
        liftIO (size c)                  >>= (`shouldBe` 3)
    it "should work without a default expiration" $ do
        c <- liftIO $ defCache Nothing
        _ <- liftIO $ expire defExpiration
        liftIO (lookup' c (fst notAvailable)) >>= (`shouldBe` Nothing)
        liftIO (lookup' c (fst ok)          ) >>= (`shouldBe` Just (snd ok))
        liftIO (lookup' c (fst notExpired)  ) >>= (`shouldBe` Just (snd notExpired))
        liftIO (lookup' c (fst expired)     ) >>= (`shouldBe` Nothing)
        liftIO (lookup' c (fst autoExpired) ) >>= (`shouldBe` Just (snd autoExpired))
    it "should work with a default expiration" $ do
        c <- liftIO $ defCache (Just defExpiration)
        _ <- liftIO $ expire defExpiration
        liftIO (lookup' c (fst notAvailable)) >>= (`shouldBe` Nothing)
        liftIO (lookup  c (fst ok)          ) >>= (`shouldBe` Just (snd ok))
        liftIO (lookup' c (fst expired)     ) >>= (`shouldBe` Nothing)
        liftIO (lookup' c (fst autoExpired) ) >>= (`shouldBe` Nothing)
    it "should delete items" $ do
        c <- liftIO $ defCache Nothing
        _ <- liftIO $ expire defExpiration
        liftIO (size c) >>= (`shouldBe` 4)
        _ <- liftIO $ delete c (fst ok)
        liftIO (size c) >>= (`shouldBe` 3)
        liftIO (lookup' c (fst notAvailable)) >>= (`shouldBe` Nothing)
        liftIO (lookup' c (fst ok)          ) >>= (`shouldBe` Nothing)
        liftIO (lookup' c (fst notExpired)  ) >>= (`shouldBe` Just (snd notExpired))
        liftIO (lookup' c (fst expired)     ) >>= (`shouldBe` Nothing)
        liftIO (lookup' c (fst autoExpired) ) >>= (`shouldBe` Just (snd autoExpired))
    it "should copy" $ do
        c  <- liftIO $ defCache Nothing
        c' <- liftIO $ copyCache c
        _  <- liftIO $ delete c (fst ok)
        liftIO (lookup c  (fst ok)) >>= (`shouldBe` Nothing)
        liftIO (lookup c' (fst ok)) >>= (`shouldBe` Just (snd ok))
    it "should set default expiratio time" $ do
        c <- liftIO $ defCache Nothing
        defaultExpiration (setDefaultExpiration c $ Just 4) `shouldBe` Just 4
    it "should return keys" $ do
        c <- liftIO $ defCache Nothing
        liftIO (keys c) >>= (`shouldContain` [(fst ok)])
        liftIO (keys c) >>= (`shouldContain` [(fst notExpired)])

defExpiration :: TimeSpec
defExpiration = 1000000

defNotExpired :: TimeSpec
defNotExpired = 1000000000

expire :: TimeSpec -> IO ()
expire = threadDelay . fromInteger . (`div` 1000) . (* 2) . toNanoSecs

expired :: (String, Int)
expired = ("expired", 1)

notExpired :: (String, Int)
notExpired = ("not expired", 5)

autoExpired :: (String, Int)
autoExpired = ("auto expired", 4)

notAvailable :: (String, Int)
notAvailable = ("not available", 2)

ok :: (String, Int)
ok = ("ok", 3)

defCache :: Maybe TimeSpec -> IO (Cache String Int)
defCache t = do
    c <- newCache t
    _ <- uncurry (insert' c Nothing)              ok
    _ <- uncurry (insert' c $ Just defExpiration) expired
    _ <- uncurry (insert  c)                      autoExpired
    _ <- uncurry (insert' c $ Just defNotExpired) notExpired
    return c
    
