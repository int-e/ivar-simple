-- |
-- Module      : Data.IChan
-- Copyright   : (c) 2008 Bertram Felgenhauer
-- License     : BSD3
--
-- Maintainer  : Bertram Felgenhauer <int-e@gmx.de>
-- Stability   : experimental
-- Portability : ghc
--
-- A channel built on top of @IVar@s.
--
module Data.IChan (
    IChan,
    new,
    read,
    write,
    tryWrite,
) where

import Prelude hiding (read)
import qualified Data.IVar as IVar
import Control.Monad
import Control.Concurrent.MVar

-- |
-- An IChan is a channel built on top of @IVar@s. It's suitable for racing
-- several threads for each value in the generated sequence.
newtype IChan a = IChan (IVar.IVar (a, IChan a))

-- | Create a new channel.
new :: IO (IChan a)
new = IChan `fmap` IVar.new

-- | Returns the contents of a channel as a list.
read :: IChan a -> [a]
read (IChan as) = let (a, ic) = IVar.read as in a : read ic

-- | Write a single value to the channel.
-- Blocks if a value has already been written to the channel. Returns a
-- new channel for writing further values.
write :: IChan a -> a -> IO (IChan a)
write (IChan as) a = do
    ic <- new
    IVar.write as (a, ic)
    return ic

-- | Attempts to write a single value to the channel.
-- If a value has already been written, returns @Nothing@. Otherwise,
-- returns a new channel for writing further values.
tryWrite :: IChan a -> a -> IO (Maybe (IChan a))
tryWrite (IChan as) a = do
    ic <- new
    success <- IVar.tryWrite as (a, ic)
    return (guard success >> return ic)
