-- |
-- Module      : Data.IVar
-- Copyright   : (c) 2008 Bertram Felgenhauer
-- License     : BSD2
--
-- Maintainer  : Bertram Felgenhauer <int-e@gmx.de>
-- Stability   : experimental
-- Portability : ghc
--
-- IVars are write-once variables.
--

-- building requires  ghc -fno-state-hack -fno-cse

module Data.IVar (
    IVar (..),
    newIVar,
    readIVar,
    tryReadIVar,
    writeIVar,
    tryWriteIVar,
) where

import Control.Concurrent.MVar
import System.IO.Unsafe

data IVar a = IVar (MVar ()) (MVar a) a

-- | @newIVar@
--
-- Creates a new IVar without a value.
newIVar :: IO (IVar a)
newIVar = do
    lock <- newMVar ()
    trans <- newEmptyMVar
    let {-# NOINLINE value #-}
        value = unsafePerformIO $ takeMVar trans
    return (IVar lock trans value)

-- | @readIVar ivar@
--
-- Returns the value of an @IVar@. The evaluation will block if there is
-- no value yet.
readIVar :: IVar a -> a
readIVar (IVar _ _ value) = value

-- | @tryReadIVar ivar@
--
-- Try to read an IVar. Returns Nothing if there's not value yet.
tryReadIVar :: IVar a -> IO (Maybe a)
tryReadIVar (IVar lock _ value) = do
    empty <- isEmptyMVar lock
    if empty then return (Just value) else return Nothing

-- | @writeIVar ivar value@
--
-- Writes a value to an IVar. Blocks if the IVar is full.
writeIVar :: IVar a -> a -> IO ()
writeIVar (IVar lock trans _) value = do
    takeMVar lock
    putMVar trans value

-- | @tryWriteIVar ivar value@
--
-- Writes a value to an IVar. Returns @True@ if successful.
tryWriteIVar :: IVar a -> a -> IO Bool
tryWriteIVar (IVar lock trans _) value = do
    a <- tryTakeMVar lock
    case a of
        Just _  -> putMVar trans value >> return True
        Nothing -> return False
