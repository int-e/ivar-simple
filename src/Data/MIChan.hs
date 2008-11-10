-- |
-- Module      : Data.MIChan
-- Copyright   : (c) 2008 Bertram Felgenhauer
-- License     : BSD3
--
-- Maintainer  : Bertram Felgenhauer <int-e@gmx.de>
-- Stability   : experimental
-- Portability : ghc
--
-- A multicast channel built on top of @IVar@s.
--

{-
comparison with Control.Concurrent.Chan:
\/ = implemented    -- = can't be implemented    nn = not needed

\/   newChan          (newMIChan)
\/   writeChan        (writeMIChan)
--   readChan         there is no way to steal items from other readers
--   unGetChan
--   isEmptyChan      needs an IO interface for reading
\/   getChanContents  (readMIchan)
\/   writeList2Chan   (writeList2MIChan)
nn   dupChan
\/   Chan             (MIChan)
-}

module Data.MIChan (
    MIChan,
    new,
    read,
    write,
    writeList,
) where

import Prelude hiding (read)
import qualified Data.IChan as IChan
import Control.Monad
import Control.Concurrent.MVar

-- |
-- An MIChan is a multicast channel. Several threads can write to the channel;
-- their values will be serialized.
newtype MIChan a = MIChan (MVar (IChan.IChan a))

-- | @newMIChan@
--
-- Create a new multicast channel.
new :: IO (MIChan a)
new = do
    ic <- IChan.new
    MIChan `fmap` newMVar ic

-- | @readMIChan chan@
--
-- Return the list of values that the channel represents.
read :: MIChan a -> IO [a]
read (MIChan mic) = do
    IChan.read `fmap` readMVar mic

-- | @writeMIChan chan value@
--
-- Send a value across the channel.
write :: MIChan a -> a -> IO ()
write (MIChan mic) value = do
    modifyMVar_ mic (\ic -> IChan.write ic value)

-- | @writeList2MIChan chan values@
--
-- Send values across the channel, atomically.
writeList :: MIChan a -> [a] -> IO ()
writeList (MIChan mic) values = do
    modifyMVar_ mic (\ic -> foldM IChan.write ic values)
