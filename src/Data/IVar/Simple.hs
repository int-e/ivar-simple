{-# LANGUAGE DeriveDataTypeable, CPP #-}
-- |
-- Module      : Data.IVar.Simple
-- Copyright   : (c) 2008-2015 Bertram Felgenhauer
-- License     : BSD3
--
-- Maintainer  : Bertram Felgenhauer <int-e@gmx.de>
-- Stability   : experimental
-- Portability : ghc
--
-- 'IVar's are write-once variables.
--
-- Similarily to 'MVar's, 'IVar's can be either empty or filled. Once filled,
-- they keep their value indefinitely - they are immutable.
--
-- Reading from an empty 'IVar' will block until the 'IVar' is filled. Because
-- the value read will never change, this is a pure computation.
--
module Data.IVar.Simple (
    IVar,
    new,
    newFull,
    read,
    tryRead,
    write,
    tryWrite,
    BlockedIndefinitelyOnIVar,
) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Typeable
import System.IO.Unsafe
import Prelude hiding (read)

-- | A write-once (/immutable/) Variable
data IVar a = IVar (MVar ()) (MVar a) a

instance Eq (IVar a) where
    IVar lock1 _ _ == IVar lock2 _ _ = lock1 == lock2
    -- note: it would be possible to generalize the type to
    -- eqIVar :: IVar a -> IVar b -> Bool  (but would that be useful?)

-- | Create a new, empty 'IVar'.
new :: IO (IVar a)
new = do
    lock <- newMVar ()
    trans <- newEmptyMVar
    let {-# NOINLINE value #-}
        value = unsafePerformIO $ takeMVar trans
    return (IVar lock trans value)

-- | Create a new filled 'IVar'.
--
-- This is slightly cheaper than creating a new 'IVar' and then writing to it.
newFull :: a -> IO (IVar a)
newFull value = do
    lock <- newEmptyMVar
    return (IVar lock (error "unused MVar") value)

-- | Returns the value of an 'IVar'.
--
-- The evaluation will block until a value is written to the 'IVar' if it
-- has no value yet.
read :: IVar a -> a
read (IVar _ _ value) = value

-- | Try to read an 'IVar'. Returns 'Nothing' if it has no value yet.
tryRead :: IVar a -> IO (Maybe a)
tryRead (IVar lock _ value) = do
    empty <- isEmptyMVar lock
    if empty then return (Just value) else return Nothing

-- | Writes a value to an 'IVar'. Raises a 'BlockedIndefinitelyOnIVar'
-- exception if the variable already has a value.
write :: IVar a -> a -> IO ()
write ivar value = do
    result <- tryWrite ivar value
    when (not result) $ throwIO BlockedIndefinitelyOnIVar
-- Note: It would be easier to block forever when the IVar is full. However,
-- the thread would likely not be garbage collected then.

-- | Writes a value to an 'IVar'. Returns 'True' if successful, and
-- 'False' otherwise.
tryWrite :: IVar a -> a -> IO Bool
tryWrite (IVar lock trans _) value = block $ do
    a <- tryTakeMVar lock
    case a of
        Just _  -> putMVar trans value >> return True
        Nothing -> return False
#if __GLASGOW_HASKELL__ >= 708
  where
    block = mask_
#endif

-- | The thread has attempted to write to a full 'IVar'.
data BlockedIndefinitelyOnIVar = BlockedIndefinitelyOnIVar
    deriving (Typeable)

instance Exception BlockedIndefinitelyOnIVar

instance Show BlockedIndefinitelyOnIVar where
    showsPrec _ BlockedIndefinitelyOnIVar =
        showString "thread blocked indefinitely writing full IVar"

