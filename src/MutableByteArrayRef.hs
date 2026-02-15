{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module MutableByteArrayRef (MutableByteArrayRef#, MutableByteArrayRef (..), read, write, new) where

import Data.Array.Byte (MutableByteArray (MutableByteArray))
import Data.Kind (Type)
import Foreign (sizeOf)
import GHC.Exts (Array#, Int (I#), Int#, Int8#, MutableArray#, MutableByteArray#, RealWorld, UnliftedType, Word8#, newArray#, newByteArray#, readArray#, readIntArray#, resizeMutableByteArray#, sizeofMutableByteArray#, writeArray#, writeIntArray#, writeWord8Array#)
import GHC.ST (ST (..))
import Prelude hiding (read)

type MutableByteArrayRef# :: Type -> UnliftedType
newtype MutableByteArrayRef# s = MutableByteArrayRef#
    { ref :: MutableArray# s (MutableByteArray# s)
    }
data MutableByteArrayRef s = MutableByteArrayRef (MutableByteArrayRef# s)

{-# INLINE read #-}
read :: MutableByteArrayRef# s -> ST s (MutableByteArray s)
read (MutableByteArrayRef#{ref}) = ST $ \s -> do
    let !(# s', array #) = readArray# ref 0# s
    (# s', MutableByteArray array #)

write :: MutableByteArrayRef# s -> MutableByteArray# s -> ST s ()
write (MutableByteArrayRef#{ref}) array = ST $ \s -> do
    let s' = writeArray# ref 0# array s
    (# s', () #)

new :: MutableByteArray# s -> ST s (MutableByteArrayRef s)
new initial = ST $ \s -> do
    let !(# s', array #) = newArray# 1# initial s
    (# s', MutableByteArrayRef (MutableByteArrayRef#{ref = array}) #)
