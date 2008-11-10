-- |
-- Module      : Data.MIChan
-- Copyright   : (c) 2008 Bertram Felgenhauer
-- License     : BSD2
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
    newMIChan,
    readMIChan,
    writeMIChan,
    writeList2MIChan,
) where

import Data.IChan
import Control.Monad
import Control.Concurrent.MVar

-- |
-- An MIChan is a multicast channel. Several threads can write to the channel;
-- their values will be serialized.
newtype MIChan a = MIChan (MVar (IChan a))

-- | @newMIChan@
--
-- Create a new multicast channel.
newMIChan :: IO (MIChan a)
newMIChan = do
    ic <- newIChan
    MIChan `fmap` newMVar ic

-- | @readMIChan chan@
--
-- Return the list of values that the channel represents.
readMIChan :: MIChan a -> IO [a]
readMIChan (MIChan mic) = do
    readIChan `fmap` readMVar mic

-- | @writeMIChan chan value@
--
-- Send a value across the channel.
writeMIChan :: MIChan a -> a -> IO ()
writeMIChan (MIChan mic) value = do
    modifyMVar_ mic (\ic -> writeIChan ic value)

-- | @writeList2MIChan chan values@
--
-- Send values across the channel, atomically.
writeList2MIChan :: MIChan a -> [a] -> IO ()
writeList2MIChan (MIChan mic) values = do
    modifyMVar_ mic (\ic -> foldM writeIChan ic values)
