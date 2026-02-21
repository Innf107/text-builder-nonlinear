-- |
-- Copyright:   (c) 2022 Andrew Lelechenko
--                  2025 Alice Schneider
-- Licence:     BSD3
-- Maintainer:  Alice Schneider <alice@welltypedwit.ch>
--
-- Adapted from the text-builder-linear benchmarks

module Main where

import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer

-- import BenchChar
-- import BenchDecimal
-- import BenchDouble
-- import BenchHexadecimal
-- import BenchText
import BenchPrettyListShortText (benchPrettyListShortText)

main :: IO ()
main = defaultMain $ map (mapLeafBenchmarks addCompare) $
  [ benchPrettyListShortText
  -- , benchText
  -- , benchChar
  -- , benchDecimal
  -- , benchHexadecimal
  -- , benchDouble
  ]

textBenchName :: String
textBenchName = "Data.Text.Lazy.Builder"

addCompare :: ([String] -> Benchmark -> Benchmark)
addCompare (name : path)
  | name /= textBenchName = bcompare (printAwkExpr (locateBenchmark (textBenchName : path)))
addCompare _ = id