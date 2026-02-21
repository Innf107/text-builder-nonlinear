{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NumDecimals #-}

module BenchPrettyListShortText where

import Data.Text (Text)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

import Builder qualified
import Data.Text qualified as Text
import Data.Text.Builder.Linear.Buffer qualified as Linear

prettyListNonLinear :: Int -> Text
prettyListNonLinear size = go (Builder.addText "[" $ Builder.unboxBuilder Builder.empty) (makeList size)
  where
    go :: Builder.UnboxedBuilder -> [Text] -> Text
    go builder [] = Builder.toText (Builder.addText "]" builder)
    go builder [last] = Builder.toText (Builder.addText "]" $ Builder.addText last builder)
    go builder (text : rest) = go (Builder.addText ", " $ Builder.addText text builder) rest

prettyListLinear :: Int -> Text
prettyListLinear size = Linear.runBuffer (\buffer -> go ("[" Linear.<| buffer) (makeList size))
  where
    go :: Linear.Buffer %1 -> [Text] -> Linear.Buffer
    go !acc [] = acc Linear.|> "]"
    go !acc [last] = acc Linear.|> last Linear.|> "]"
    go !acc (text : rest) = go (acc Linear.|> text Linear.|> ", ") rest

benchPrettyListShortText :: Benchmark
benchPrettyListShortText = bgroup "Text" $ map mkGroup [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6]

makeList :: Int -> [Text]
makeList size = replicate size (Text.replicate size "persistence is nice")
{-# NOINLINE makeList #-}

mkGroup :: Int -> Benchmark
mkGroup size =
    bgroup
        (show size)
        [ bench "text-builder-nonlinear" (nf prettyListNonLinear size)
        , bench "text-builder-linear" (nf prettyListLinear size)
        ]
