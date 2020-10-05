{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Data.IVar.Simple
-- Copyright   : (c) 2008-2020 Bertram Felgenhauer
-- License     : MIT
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
data IVar a = IVar (MVar a) a

instance Eq (IVar a) where
    IVar trans1 _ == IVar trans2 _ = trans1 == trans2
    -- note: it would be possible to generalize the type to
    -- eqIVar :: IVar a -> IVar b -> Bool  (but would that be useful?)

-- | Create a new, empty 'IVar'.
new :: IO (IVar a)
new = do
    trans <- newEmptyMVar
    let {-# NOINLINE value #-}
        value = unsafePerformIO $ readMVar trans
    return (IVar trans value)

-- | Create a new filled 'IVar'.
--
-- This is slightly cheaper than creating a new 'IVar' and then writing to it.
newFull :: a -> IO (IVar a)
newFull value = do
    trans <- newMVar (error "unused value")
    return (IVar trans value)

-- | Returns the value of an 'IVar'.
--
-- The evaluation will block until a value is written to the 'IVar' if it
-- has no value yet.
read :: IVar a -> a
read (IVar _ value) = value

-- | Try to read an 'IVar'. Returns 'Nothing' if it has no value yet.
tryRead :: IVar a -> IO (Maybe a)
tryRead (IVar trans value) = do
    empty <- isEmptyMVar trans
    if not empty then return (Just value) else return Nothing

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
tryWrite (IVar trans _) value = tryPutMVar trans value

-- | The thread has attempted to write to a full 'IVar'.
data BlockedIndefinitelyOnIVar = BlockedIndefinitelyOnIVar
    deriving (Typeable)

instance Exception BlockedIndefinitelyOnIVar

instance Show BlockedIndefinitelyOnIVar where
    showsPrec _ BlockedIndefinitelyOnIVar =
        showString "thread blocked indefinitely writing full IVar"

