-- |
-- Module      : Data.IChan
-- Copyright   : (c) 2008 Bertram Felgenhauer
-- License     : BSD2
--
-- Maintainer  : Bertram Felgenhauer <int-e@gmx.de>
-- Stability   : experimental
-- Portability : ghc
--
-- Two channels built on top of @IVar@s.
--
module Data.IChan (
    IChan,
    newIChan,
    readIChan,
    writeIChan,
    tryWriteIChan,
) where

import Data.IVar
import Control.Monad
import Control.Concurrent.MVar

-- |
-- An IChan is a channel built on top of @IVar@s. It's suitable for racing
-- several threads for each value in the generated sequence.
newtype IChan a = IChan (IVar (a, IChan a))

-- | @newIChan@
--
-- Create a new channel.
newIChan :: IO (IChan a)
newIChan = IChan `fmap` newIVar

-- | @readIChan ichan@
--
-- Returns the contents of a channel as a list.
readIChan :: IChan a -> [a]
readIChan (IChan as) = let (a, ic) = readIVar as in a : readIChan ic

-- | @tryWriteIChan ichan value@
--
-- Write a single value to the channel. Blocks if a value has already been
-- written to the channel. Returns a new channel for writing further values.
writeIChan :: IChan a -> a -> IO (IChan a)
writeIChan (IChan as) a = do
    ic <- newIChan
    writeIVar as (a, ic)
    return ic

-- | @tryWriteIChan ichan value@
--
-- Attempts to write a single value to the channel. If a channel had already
-- been written, returns @Nothing@. Otherwise, returns a new channel for
-- writing further values.
tryWriteIChan :: IChan a -> a -> IO (Maybe (IChan a))
tryWriteIChan (IChan as) a = do
    ic <- newIChan
    success <- tryWriteIVar as (a, ic)
    return (guard success >> return ic)
