{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Builder
import qualified Data.Text as Text
import Control.Monad.ST (stToIO)
import qualified MutableBuilder

main :: IO ()
main = do
        builder <- stToIO $ MutableBuilder.newWithCapacity 4
        print =<< stToIO (MutableBuilder.debugBuilder builder)
        stToIO $ MutableBuilder.addText builder "\172735\1076202a"
        print =<< stToIO (MutableBuilder.toText builder)
        print =<< stToIO (MutableBuilder.debugBuilder builder)
        stToIO $ MutableBuilder.addText builder ""
        print =<< stToIO (MutableBuilder.toText builder)
        print =<< stToIO (MutableBuilder.debugBuilder builder)

