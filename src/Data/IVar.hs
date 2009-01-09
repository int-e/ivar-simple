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
    IVar,
    new,
    newFull,
    read,
    tryRead,
    write,
    tryWrite,
) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import Prelude hiding (read)

-- | A write-once (\'immutable\') Variable
data IVar a = IVar (MVar ()) (MVar a) a

-- | Creates a new, empty IVar.
new :: IO (IVar a)
new = do
    lock <- newMVar ()
    trans <- newEmptyMVar
    let {-# NOINLINE value #-}
        value = unsafePerformIO $ takeMVar trans
    return (IVar lock trans value)

-- | Create a new filled IVar.
--
-- This is slightly cheaper than creating a new @IVar@ and then writing to it.
newFull :: a -> IO (IVar a)
newFull value = do
    lock <- newEmptyMVar
    return (IVar lock (error "unused MVar") value)

-- | Returns the value of an @IVar@.
--
-- The evaluation of the returned value will block until a value is written to
-- the @IVar@ if there is no value yet.
--
-- @read@ itself will not block.
read :: IVar a -> a
read (IVar _ _ value) = value

-- | Try to read an IVar. Returns Nothing if there's not value yet.
tryRead :: IVar a -> IO (Maybe a)
tryRead (IVar lock _ value) = do
    empty <- isEmptyMVar lock
    if empty then return (Just value) else return Nothing

-- | Writes a value to an IVar. Blocks if the IVar is full.
write :: IVar a -> a -> IO ()
write (IVar lock trans _) value = do
    takeMVar lock
    putMVar trans value

-- | Writes a value to an IVar. Returns @True@ if successful.
tryWrite :: IVar a -> a -> IO Bool
tryWrite (IVar lock trans _) value = do
    a <- tryTakeMVar lock
    case a of
        Just _  -> putMVar trans value >> return True
        Nothing -> return False
