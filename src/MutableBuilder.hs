{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module MutableBuilder (MutableBuilder, newWithCapacity, fillSize, addByte, copyNonOverlappingUnsafe, copyToByteArrayUnchecked, copyAllToByteArrayUnchecked, debugBuilder, addText, toText) where

import Data.Array.Byte (ByteArray (ByteArray), MutableByteArray (..))
import GHC.Exts (Int (I#), Int#, MutableByteArray#, State#, Word8#, copyByteArray#, copyMutableByteArray#, copyMutableByteArrayNonOverlapping#, getSizeofMutableByteArray#, newByteArray#, resizeMutableByteArray#, unsafeFreezeByteArray#, writeWord8Array#, (+#))
import GHC.ST (ST (..))
import UnboxedIntRef (UnboxedIntRef (..), UnboxedIntRef#)
import UnboxedIntRef qualified

import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.Text (Text)
import Data.Text.Internal qualified as Text.Internal
import Debug.Trace (traceM)
import MutableByteArrayRef (MutableByteArrayRef (..), MutableByteArrayRef#)
import MutableByteArrayRef qualified
import System.IO.Unsafe (unsafePerformIO)

data MutableBuilder s = MkMutableBuilder
    { byteArrayRef :: MutableByteArrayRef# s
    , fillSize :: UnboxedIntRef# s
    }

resizeFactor :: Int
resizeFactor = 2

unST :: State# s -> ST s a -> (# State# s, a #)
unST s (ST f) = f s

{-# INLINE fillSize #-}
fillSize :: MutableBuilder s -> ST s Int
fillSize (MkMutableBuilder{fillSize}) = UnboxedIntRef.read fillSize

newWithCapacity :: Int -> ST s (MutableBuilder s)
newWithCapacity (I# initialSize) = do
    (MutableByteArrayRef byteArrayRef) <- ST $ \s -> do
        let !(# s', byteArray #) = newByteArray# initialSize s
        let !(# s'', byteArrayRef #) = unST s' (MutableByteArrayRef.new byteArray)
        (# s'', byteArrayRef #)
    UnboxedIntRef fillSize <- UnboxedIntRef.new 0
    pure (MkMutableBuilder{byteArrayRef, fillSize})

-- | Apply an action to the inner buffer and resize it if necessary.
-- The passed in size needs to be exactly the number of bytes that will be written to the buffer
unsafeModify :: MutableBuilder s -> Int# -> (MutableByteArray# s -> Int# -> ST s ()) -> ST s ()
unsafeModify MkMutableBuilder{byteArrayRef, fillSize} writtenAmount action = do
    (MutableByteArray byteArray) <- MutableByteArrayRef.read byteArrayRef
    capacity <- ST $ \s -> do
        let !(# s', capacity #) = getSizeofMutableByteArray# byteArray s
        (# s', I# capacity #)
    size <- UnboxedIntRef.read fillSize
    UnboxedIntRef.write fillSize (size + I# writtenAmount)
    let !(I# nextIndex) = size
    if size + I# writtenAmount < capacity
        then
            action byteArray nextIndex
        else ST $ \s -> do
            let !(I# newCapacity) = (size + I# writtenAmount) * resizeFactor
            let !(# s', newByteArray #) = resizeMutableByteArray# byteArray newCapacity s
            unST s' $ do
                MutableByteArrayRef.write byteArrayRef newByteArray
                action newByteArray nextIndex
{-# INLINE unsafeModify #-}

addByte :: MutableBuilder s -> Word8# -> ST s ()
addByte builder byte = unsafeModify builder 1# $ \array index -> ST $ \s -> do
    let s' = writeWord8Array# array index byte s
    (# s', () #)

addText :: MutableBuilder s -> Text -> ST s ()
addText builder (Text.Internal.Text (ByteArray textBytes) (I# offset) (I# length)) =
    unsafeModify builder length $ \array index -> ST $ \s -> do
        let s' = copyByteArray# textBytes offset array index length s
        (# s', () #)

{- | Append @length@ bytes from source to target starting at index @start@.
This will *not* resize the buffer and only works on buffers with enough
remaining capacity.
The arrays must not overlap
THESE CONDITIONS ARE NOT CHECKED AND WILL RESULT IN UNDEFINED BEHAVIOR IF THEY ARE VIOLATED.
-}
copyNonOverlappingUnsafe :: MutableBuilder s -> MutableBuilder s -> Int -> Int -> ST s ()
copyNonOverlappingUnsafe
    (MkMutableBuilder{byteArrayRef = sourceByteArrayRef})
    (MkMutableBuilder{byteArrayRef = targetByteArrayRef, fillSize = targetFillSize})
    (I# start)
    (I# length) = do
        MutableByteArray sourceByteArray <- MutableByteArrayRef.read sourceByteArrayRef
        MutableByteArray targetByteArray <- MutableByteArrayRef.read targetByteArrayRef
        I# offset <- UnboxedIntRef.read targetFillSize
        ST $ \s -> do
            let s' = copyMutableByteArrayNonOverlapping# sourceByteArray start targetByteArray offset length s
            (# s', () #)
        UnboxedIntRef.write targetFillSize (I# (offset +# length))
{-# INLINE copyNonOverlappingUnsafe #-}

{- | Copy @length@ bytes from the contents of this builder to a MutableByteArray#.
There need to be at least @length@ bytes in the builder and at least @length@ bytes
of spaces in the target array but NEITHER OF THESE CONDITIONS IS CHECKED.
-}
copyToByteArrayUnchecked :: MutableBuilder s -> MutableByteArray# s -> Int# -> Int# -> ST s ()
copyToByteArrayUnchecked MkMutableBuilder{byteArrayRef} targetArray targetOffset length = do
    MutableByteArray byteArray <- MutableByteArrayRef.read byteArrayRef
    ST $ \s -> do
        let s' = copyMutableByteArrayNonOverlapping# byteArray 0# targetArray targetOffset length s
        (# s', () #)

copyAllToByteArrayUnchecked :: MutableBuilder s -> MutableByteArray# s -> Int# -> ST s ()
copyAllToByteArrayUnchecked MkMutableBuilder{byteArrayRef, fillSize} targetArray targetOffset = do
    MutableByteArray byteArray <- MutableByteArrayRef.read byteArrayRef
    I# length <- UnboxedIntRef.read fillSize
    ST $ \s -> do
        let s' = copyMutableByteArrayNonOverlapping# byteArray 0# targetArray targetOffset length s
        (# s', () #)

debugBuilder :: MutableBuilder s -> ST s String
debugBuilder (MkMutableBuilder{byteArrayRef, fillSize}) = do
    size <- UnboxedIntRef.read fillSize
    MutableByteArray array <- MutableByteArrayRef.read byteArrayRef
    capacity <- ST \s -> let !(# s', size #) = getSizeofMutableByteArray# array s in (# s', I# size #)

    frozenArray <- freezeCopyOfMutableByteArray array

    pure ("{ fillSize = " <> show size <> ", capacity = " <> show capacity <> ", contents = " <> show frozenArray <> " }")

freezeCopyOfMutableByteArray :: MutableByteArray# s -> ST s ByteArray
freezeCopyOfMutableByteArray mutableByteArray = do
    ST \s -> do
        let !(# s', size #) = getSizeofMutableByteArray# mutableByteArray s
        let !(# s'', finalArray #) = newByteArray# size s'
        let s''' = copyMutableByteArray# mutableByteArray 0# finalArray 0# size s''
        let !(# s'''', frozenByteArray #) = unsafeFreezeByteArray# finalArray s'''
        (# s'''', (ByteArray frozenByteArray) #)

toText :: MutableBuilder s -> ST s Text
toText MkMutableBuilder{byteArrayRef, fillSize} = do
    I# size <- UnboxedIntRef.read fillSize
    MutableByteArray byteArray <- MutableByteArrayRef.read byteArrayRef
    ST \s -> do
        let !(# s', finalArray #) = newByteArray# size s
        let s'' = copyMutableByteArray# byteArray 0# finalArray 0# size s'
        let !(# s''', frozenByteArray #) = unsafeFreezeByteArray# finalArray s''
        (# s''', Text.Internal.Text (ByteArray frozenByteArray) 0 (I# size) #)
