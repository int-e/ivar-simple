-- |
-- Module      : Data.IVar.Simple.MIChan
-- Copyright   : (c) 2008-2012 Bertram Felgenhauer
-- License     : BSD3
--
-- Maintainer  : Bertram Felgenhauer <int-e@gmx.de>
-- Stability   : experimental
-- Portability : ghc
--
-- An 'MIChan' is a multicast channel built on top of an 'IChan.IChan'.
--
-- Like 'IChan.IChan', this channel supports multiple readers. It is
-- comparable to a @Control.Concurrent.Chan.Chan@ for the writing end:
-- Each write will append an element to the channel. No writes will fail.

module Data.IVar.Simple.MIChan (
    -- $comparison
    MIChan,
    new,
    read,
    write,
    writeList,
) where

import Prelude hiding (read)
import qualified Data.IVar.Simple.IChan as IChan
import Control.Monad
import Control.Concurrent.MVar

-- | A multicast channel.
newtype MIChan a = MIChan (MVar (IChan.IChan a))

-- | Create a new multicast channel.
new :: IO (MIChan a)
new = do
    ic <- IChan.new
    MIChan `fmap` newMVar ic

-- | Return the list of values that the channel represents.
read :: MIChan a -> IO [a]
read (MIChan mic) = do
    IChan.read `fmap` readMVar mic

-- | Send a value across the channel.
write :: MIChan a -> a -> IO ()
write (MIChan mic) value = do
    modifyMVar_ mic (\ic -> IChan.write ic value)

-- | Send several values across the channel, atomically.
writeList :: MIChan a -> [a] -> IO ()
writeList (MIChan mic) values = do
    modifyMVar_ mic (\ic -> foldM IChan.write ic values)

-- $comparison
-- Comparison to @Control.Concurrent.Chan@:
--
-- > Control.Concurrent.Chan.Chan => Data.MIChan
-- > newChan                      => new
-- > writeChan                    => write
-- > getChanContents              => read
-- > writeList2Chan               => writeList
--
-- These can't be implemented:
--
-- > readChan     (can't steal items from other readers)
-- > unGetChan    (there is no separate reading end)
-- > isEmptyChan  (needs an IO interface for reading)
-- > dupChan      (not needed)
