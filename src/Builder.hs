{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Builder where

import Data.Kind (Type)
import Foreign (sizeOf)
import GHC.Exts (Array#, Int (I#), Int#, Int8#, MutableByteArray#, RealWorld, State#, UnliftedType, Word8#, newByteArray#, readArray#, readIntArray#, resizeMutableByteArray#, sizeofMutableByteArray#, writeIntArray#, writeWord8Array#, (+#), (-#))
import GHC.ST (ST (..))
import UnboxedIntRef (UnboxedIntRef#)
import UnboxedIntRef qualified

import Control.Monad.ST.Strict (stToIO)
import MutableBuilder (MutableBuilder)
import MutableBuilder qualified
import System.IO.Unsafe (unsafePerformIO)

import Data.Array.Byte (ByteArray (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Internal qualified as Text.Internal
import GHC.Base (assert)
import GHC.Exts (copyByteArray#, unsafeFreezeByteArray#)

type PreviousSlices :: UnliftedType
data PreviousSlices
    = NoSlices
    | Slice {builder :: MutableBuilder RealWorld, sharedPrefix :: Int#, tail :: PreviousSlices}
    | FixedChunk {fixedText :: Text, tail :: PreviousSlices}
    | Concat PreviousSlices PreviousSlices

newtype UnboxedBuilder = MkBuilder (# Int#, Int#, MutableBuilder RealWorld, PreviousSlices #)

{- | The number of bytes from which on copying to the builder is more expensive than adding a new `FixedChunk` node
TODO: determine this experimentally
-}
fixedChunkThreshold :: Int
fixedChunkThreshold = 512

{- | The number of bytes up to which it is cheaper to copy the contents from the old buffer into the new one than
to add a new `Slice` node
TODO: no idea what this should be
-}
copyThreshold :: Int
copyThreshold = 64

-- | The minimum size of a newly created buffer in bytes
minBufferCapacity :: Int
minBufferCapacity = 16

{- | The number of bytes up to which it is cheaper to copy the entire text contents of another builder on append than
to add a new `Concat` node.
TODO: no clue what to choose here
-}
appendCopyThreshold :: Int
appendCopyThreshold = 128

data Builder = MkBoxedBuilder UnboxedBuilder

{- | We special case a single buffer with one element so that the first push always resizes
and we don't leak the first ever MutableBuilder
-}
emptyBaseSlice :: MutableBuilder RealWorld
emptyBaseSlice = unsafePerformIO $ stToIO $ do
    builder <- MutableBuilder.newWithCapacity 1
    MutableBuilder.addByte builder 0#Word8
    pure builder
{-# NOINLINE emptyBaseSlice #-}

empty :: Builder
empty = MkBoxedBuilder (MkBuilder (# 0#, 0#, emptyBaseSlice, NoSlices #))

debugBuilder :: UnboxedBuilder -> String
debugBuilder (MkBuilder (# ownLength, totalSize, builder, slices #)) =
    "{ ownLength = "
        <> show (I# ownLength)
        <> ", totalSize = "
        <> show (I# totalSize)
        <> ", builder = <abstract>"
        <> ", slices = "
        <> debugPreviousSlices slices
        <> " }"

debugPreviousSlices :: PreviousSlices -> String
debugPreviousSlices = \case
    NoSlices -> "NoSlices"
    Slice{builder, sharedPrefix, tail} ->
        "Slice { builder = <abstract>" <> ", sharedPrefix = " <> show (I# sharedPrefix) <> "} -> " <> debugPreviousSlices tail
    FixedChunk{fixedText, tail} ->
        "FixedChunk { fixedText = " <> show fixedText <> "} -> " <> debugPreviousSlices tail
    Concat left right ->
        "Concat { left = " <> debugPreviousSlices left <> " | right = " <> debugPreviousSlices right <> " }"

unboxBuilder :: Builder -> UnboxedBuilder
unboxBuilder (MkBoxedBuilder builder) = builder

{-# INLINE unsafeModify #-}
unsafeModify :: UnboxedBuilder -> Int# -> (forall s. MutableBuilder s -> ST s ()) -> UnboxedBuilder
unsafeModify builder sizeHint action = do
    let !(MkBoxedBuilder unboxedBuilder) = unsafePerformIO $ stToIO $ unsafeModifyST builder sizeHint action
    unboxedBuilder

{- | Modify a builder by applying a function to the mutable builder.
The size hint will be used to determine the size added by the action and avoid unnecessary resizing.
It does not need to be exact.
Set it to 0 to disable size hints
-}
{-# INLINE unsafeModifyST #-}
unsafeModifyST :: UnboxedBuilder -> Int# -> (forall s. MutableBuilder s -> ST s ()) -> ST RealWorld Builder
unsafeModifyST (MkBuilder (# ownSize, totalSize, sharedBuilder, previousSlices #)) sizeHint action = do
    sharedSize <- MutableBuilder.fillSize sharedBuilder
    if (sharedSize == I# ownSize)
        then do
            action sharedBuilder
            I# newSize <- MutableBuilder.fillSize sharedBuilder
            pure (MkBoxedBuilder (MkBuilder (# newSize, totalSize +# (newSize -# ownSize), sharedBuilder, previousSlices #)))
        else do
            let newBufferCapacity = min (I# (ownSize +# sizeHint) * 2) minBufferCapacity
            newBuffer <- MutableBuilder.newWithCapacity newBufferCapacity
            if I# ownSize < copyThreshold
                then do
                    MutableBuilder.copyNonOverlappingUnsafe sharedBuilder newBuffer 0 (I# ownSize)
                    action newBuffer
                    I# newSize <- MutableBuilder.fillSize newBuffer
                    pure (MkBoxedBuilder (MkBuilder (# newSize, totalSize +# (newSize -# ownSize), newBuffer, previousSlices #)))
                else do
                    let slices = Slice sharedBuilder ownSize previousSlices
                    action newBuffer
                    I# newSize <- MutableBuilder.fillSize newBuffer
                    pure (MkBoxedBuilder (MkBuilder (# newSize, totalSize +# newSize, newBuffer, slices #)))

-- | Add a raw byte to the builder. It is NOT checked if this results in valid UTF-8
addByteUnchecked :: Word8# -> UnboxedBuilder -> UnboxedBuilder
addByteUnchecked byte builder = unsafeModify builder 1# (\mutable -> MutableBuilder.addByte mutable byte)

addChar :: Char -> UnboxedBuilder -> UnboxedBuilder
-- This may add anything from 1 to 4 bytes but since most text is ASCII it is probably most efficient to use a size hint of 1.
-- Even if all chars have size 4, this will only result in slightly more resize operations than necessary
addChar char builder = unsafeModify builder 1# (\mutable -> undefined)

textSizeInBytes :: Text -> Int
textSizeInBytes (Text.Internal.Text _ _ size) = size

addText :: Text -> UnboxedBuilder -> UnboxedBuilder
addText text builder@(MkBuilder (# ownSize, totalSize, sharedBuffer, previousSlices #)) = do
    let !(I# textSize) = textSizeInBytes text
    if I# textSize < fixedChunkThreshold
        then unsafeModify builder textSize $ \mutable -> MutableBuilder.addText mutable text
        else do
            let !newBuffer = unsafePerformIO $ stToIO $ MutableBuilder.newWithCapacity minBufferCapacity
            let !slices = FixedChunk text (Slice sharedBuffer ownSize previousSlices)

            MkBuilder (# 0#, totalSize +# textSize, newBuffer, slices #)

unST :: State# s -> ST s a -> (# State# s, a #)
unST s (ST f) = f s

toText :: UnboxedBuilder -> Text
toText (MkBuilder (# ownSize, totalSize, lastBuffer, previousSlices #)) = unsafePerformIO $ stToIO $ ST $ \s -> do
    let !(# s', finalArray #) = newByteArray# totalSize s
    let !(# s'', () #) = unST s' $ do
            lastIndex <- writeSlices finalArray 0# previousSlices
            assert (lastIndex + I# ownSize == I# totalSize) (pure ())
            writeLastBuffer finalArray lastIndex

    let !(# s''', frozenArray #) = unsafeFreezeByteArray# finalArray s''
    (# s''', Text.Internal.Text (ByteArray frozenArray) 0 (I# totalSize) #)
  where
    writeSlices :: MutableByteArray# RealWorld -> Int# -> PreviousSlices -> ST RealWorld Int
    writeSlices finalArray index = \case
        NoSlices -> pure (I# index)
        Concat left right -> do
            I# indexAfterLeft <- writeSlices finalArray index left
            indexAfterRight <- writeSlices finalArray indexAfterLeft right
            pure indexAfterRight
        FixedChunk{fixedText, tail} -> do
            I# ownIndex <- writeSlices finalArray index tail
            let !(Text.Internal.Text (ByteArray textBuffer) (I# textOffset) (I# textLength)) = fixedText
            ST $ \s -> do
                let s' = copyByteArray# textBuffer textOffset finalArray ownIndex textLength s
                (# s', () #)
            pure (I# (ownIndex +# textLength))
        Slice{builder, sharedPrefix, tail} -> do
            I# ownIndex <- writeSlices finalArray index tail
            MutableBuilder.copyToByteArrayUnchecked builder finalArray ownIndex sharedPrefix
            pure (I# (ownIndex +# sharedPrefix))

    writeLastBuffer :: MutableByteArray# RealWorld -> Int -> ST RealWorld ()
    writeLastBuffer finalArray (I# index) = do
        MutableBuilder.copyToByteArrayUnchecked lastBuffer finalArray index ownSize