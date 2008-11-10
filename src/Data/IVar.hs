-- |
-- Module      : Data.IVar
-- Copyright   : (c) 2008 Bertram Felgenhauer
-- License     : BSD3
--
-- Maintainer  : Bertram Felgenhauer <int-e@gmx.de>
-- Stability   : experimental
-- Portability : ghc
--
-- IVars are write-once variables.
--

module Data.IVar (
    IVar (..),
    new,
    read,
    tryRead,
    write,
    tryWrite,
) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import Prelude hiding (read)

data IVar a = IVar (MVar ()) (MVar a) a

-- | @new@
--
-- Creates a new IVar without a value.
new :: IO (IVar a)
new = do
    lock <- newMVar ()
    trans <- newEmptyMVar
    let {-# NOINLINE value #-}
        value = unsafePerformIO $ takeMVar trans
    return (IVar lock trans value)

-- | @readIVar ivar@
--
-- Returns the value of an @IVar@. The evaluation will block if there is
-- no value yet.
read :: IVar a -> a
read (IVar _ _ value) = value

-- | @tryReadIVar ivar@
--
-- Try to read an IVar. Returns Nothing if there's not value yet.
tryRead :: IVar a -> IO (Maybe a)
tryRead (IVar lock _ value) = do
    empty <- isEmptyMVar lock
    if empty then return (Just value) else return Nothing

-- | @writeIVar ivar value@
--
-- Writes a value to an IVar. Blocks if the IVar is full.
write :: IVar a -> a -> IO ()
write (IVar lock trans _) value = do
    takeMVar lock
    putMVar trans value

-- | @tryWriteIVar ivar value@
--
-- Writes a value to an IVar. Returns @True@ if successful.
tryWrite :: IVar a -> a -> IO Bool
tryWrite (IVar lock trans _) value = do
    a <- tryTakeMVar lock
    case a of
        Just _  -> putMVar trans value >> return True
        Nothing -> return False
