{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module UnboxedIntRef (UnboxedIntRef#, UnboxedIntRef(..), write, read, new, modify) where

import Prelude hiding (read)
import Data.Kind (Type)
import Foreign (sizeOf)
import GHC.Exts (Array#, Int (I#), Int#, Int8#, MutableByteArray#, RealWorld, UnliftedType, Word8#, newByteArray#, readIntArray#, resizeMutableByteArray#, sizeofMutableByteArray#, writeIntArray#, writeWord8Array#, readArray#)
import GHC.ST (ST (..))


type UnboxedIntRef# :: Type -> UnliftedType
newtype UnboxedIntRef# s = UnboxedIntRef#
    { ref :: MutableByteArray# s
    }
data UnboxedIntRef s = UnboxedIntRef (UnboxedIntRef# s)

{-# INLINE write #-}
write :: UnboxedIntRef# s -> Int -> ST s ()
write (UnboxedIntRef#{ref}) (I# int) = ST $ \s -> do
    let s' = writeIntArray# ref 0# int s
    (# s', () #)

{-# INLINE read #-}
read :: UnboxedIntRef# s -> ST s Int
read (UnboxedIntRef#{ref}) = ST $ \s -> do
    let !(# s', int# #) = readIntArray# ref 0# s
    (# s', I# int# #)

{-# INLINE new #-}
new :: Int -> ST s (UnboxedIntRef s)
new (I# initial) = ST $ \s -> do
    let !(I# intSize) = sizeOf (undefined :: Int)
    let !(# s', array #) = newByteArray# intSize s
    let s'' = writeIntArray# array 0# initial s'
    (# s'', (UnboxedIntRef (UnboxedIntRef# array)) #)

{-# INLINE modify #-}
modify :: UnboxedIntRef# s -> (Int -> Int) -> ST s ()
modify ref f = do
    value <- read ref
    write ref (f value)