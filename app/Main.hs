{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Builder

main :: IO ()
main = do
    print (Builder.toText (Builder.addText "aaa" (Builder.unboxBuilder Builder.empty)))
