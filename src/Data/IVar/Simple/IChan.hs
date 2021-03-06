-- |
-- Module      : Data.IVar.Simple.IChan
-- Copyright   : (c) 2008-2020 Bertram Felgenhauer
-- License     : MIT
--
-- Maintainer  : Bertram Felgenhauer <int-e@gmx.de>
-- Stability   : experimental
-- Portability : ghc
--
-- An 'IChan's is a type of multicast channel built on top of 'IVar.IVar's.
-- It supports multiple readers. The channel is represented as a linked
-- list. The 'IChan' data type represents the head of a channel.
--
-- Writing to an 'IChan' head has write-once semantics similar to 'IVar.IVar's:
-- only the first of several attempts to write to the head will succeed,
-- returning a new 'IChan' head for writing more values.
--
module Data.IVar.Simple.IChan (
    IChan,
    new,
    read,
    uncons,
    write,
    tryWrite,
) where

import Prelude hiding (read)
import qualified Data.IVar.Simple as IVar
import Control.Monad
import Control.Concurrent.MVar

-- | A channel head
newtype IChan a = IChan (IVar.IVar (a, IChan a))
    deriving Eq

-- | Create a new channel.
new :: IO (IChan a)
new = IChan `fmap` IVar.new

-- | Returns the contents of a channel as a list, starting at the channel
-- head.
--
-- This is a pure computation. Forcing elements of the list may, however,
-- block.
read :: IChan a -> [a]
read ic = let (a, ic') = uncons ic in a : read ic'

-- | Split channel into head and tail.
--
-- This is a pure operation, but it may block.
--
-- @since 0.3.3
uncons :: IChan a -> (a, IChan a)
uncons (IChan as) = IVar.read as

-- | Write a single value to the channel.
--
-- Raises a 'BlockedIndefinitelyOnIVar' exception if a value has already
-- been written to the channel. Otherwise, returns a new channel head for
-- writing further values.
write :: IChan a -> a -> IO (IChan a)
write (IChan as) a = do
    ic <- new
    IVar.write as (a, ic)
    return ic

-- | Attempts to write a single value to the channel.
--
-- If a value has already been written, returns 'Nothing'. Otherwise,
-- returns a new channel head for writing further values.
tryWrite :: IChan a -> a -> IO (Maybe (IChan a))
tryWrite (IChan as) a = do
    ic <- new
    success <- IVar.tryWrite as (a, ic)
    return (guard success >> return ic)
