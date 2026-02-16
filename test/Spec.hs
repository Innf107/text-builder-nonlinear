{-# LANGUAGE BlockArguments #-}

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck qualified as QuickCheck

import Builder (Builder (MkBoxedBuilder))
import Builder qualified
import MutableBuilder qualified

import Control.Monad.ST.Strict (runST)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty.HUnit ((@=?), (@?=))
import Test.Tasty.QuickCheck ((===))

main :: IO ()
main =
    Tasty.defaultMain $
        Tasty.testGroup
            "Tests"
            [ builderUnitTests
            , builderPropertyTests
            , mutableBuilderPropertyTests
            , mutableBuilderUnitTests
            ]

builderUnitTests :: TestTree
builderUnitTests =
    Tasty.testGroup
        "Builder Unit tests"
        [ HUnit.testCase "empty builder" $ Builder.toText (Builder.unboxBuilder Builder.empty) @=? ""
        , HUnit.testCase "linear short" do
            let builder = Builder.addText "ccc" $ Builder.addText "bbb" $ Builder.addText "aaa" $ Builder.unboxBuilder (Builder.empty)
            Builder.toText builder @?= "aaabbbccc"
        , {-, HUnit.testCase "add single char text" do
              let builder = Builder.addText "a" $ Builder.unboxBuilder Builder.empty
              Builder.toText builder @?= "a"-}
          HUnit.testCase "linear fixed chunk" do
            let builder = Builder.addText (Text.replicate 1000 "x") $ Builder.addText "short" (Builder.unboxBuilder (Builder.empty))
            Builder.toText builder @?= ("short" <> Text.replicate 1000 "x")
        , HUnit.testCase "persistent short" do
            let builder = Builder.addText "xxx" (Builder.unboxBuilder Builder.empty)
            let derived = [MkBoxedBuilder $ Builder.addText "aaa" builder, MkBoxedBuilder $ Builder.addText "bbb" builder, MkBoxedBuilder $ Builder.addText "ddd" (Builder.addText "ccc" builder)]
            map (\(MkBoxedBuilder builder) -> Builder.toText builder) derived @=? ["xxxaaa", "xxxbbb", "xxxcccddd"]
            {-, HUnit.testCase "failing quickcheck" do
                let appendTexts builder = \case
                        [] -> Builder.toText builder
                        (text : rest) -> appendTexts (Builder.addText text builder) rest
                 in do
                        let texts = ["", "a"]
                        mconcat texts @=? appendTexts (Builder.unboxBuilder Builder.empty) texts -}
        ]

builderPropertyTests :: TestTree
builderPropertyTests =
    Tasty.testGroup
        "Builder property tests"
        [ QuickCheck.testProperty "linearly appending arbitrary Texts behaves like mconcat" $
            -- Manual fold because we're working with an unboxed builder here
            let appendTexts builder = \case
                    [] -> Builder.toText builder
                    (text : rest) -> appendTexts (Builder.addText text builder) rest
             in \(strings :: [String]) -> do
                    let texts = map Text.pack strings
                    mconcat texts === appendTexts (Builder.unboxBuilder Builder.empty) texts
        ]

mutableBuilderUnitTests :: TestTree
mutableBuilderUnitTests =
    Tasty.testGroup
        "MutableBuilder unit tests"
        [ HUnit.testCase "empty mutable builder" $ do
            let output = runST do
                    builder <- MutableBuilder.newWithCapacity 4
                    MutableBuilder.toText builder
            output @?= ""
        , HUnit.testCase "addText no resize" $ do
            let output = runST do
                    builder <- MutableBuilder.newWithCapacity 16
                    MutableBuilder.addText builder "aaa"
                    MutableBuilder.addText builder "bbb"
                    MutableBuilder.addText builder "ccc"
                    MutableBuilder.toText builder
            output @?= "aaabbbccc"
        , HUnit.testCase "addText resize" $ do
            let output = runST do
                    builder <- MutableBuilder.newWithCapacity 3
                    MutableBuilder.addText builder "aaa"
                    MutableBuilder.addText builder "bbb"
                    MutableBuilder.addText builder "ccc"
                    MutableBuilder.toText builder
            output @?="aaabbbccc"
        , HUnit.testCase "addText almost resize" do
            let output = runST do
                    builder <- MutableBuilder.newWithCapacity 4
                    MutableBuilder.addText builder "aaa"
                    MutableBuilder.addText builder "bbb"
                    MutableBuilder.addText builder "ccc"
                    MutableBuilder.toText builder
            output @?= "aaabbbccc"
        , HUnit.testCase "quickcheck failure" do
            let output = runST do
                    builder <- MutableBuilder.newWithCapacity 4
                    MutableBuilder.addText builder ","
                    MutableBuilder.addText builder "\ENQ\7176\8772\1059304"
                    MutableBuilder.addText builder "xH\1087738b"
                    MutableBuilder.addText builder ""
                    MutableBuilder.addText builder "\SUB\174289N\1019881\172897"
                    MutableBuilder.toText builder
            output @?= ",\ENQ\7176\8772\1059304xH\1087738b\SUB\174289N\1019881\172897"
        , HUnit.testCase "quickcheck failure 2" do
            let output = runST do
                    builder <- MutableBuilder.newWithCapacity 4
                    MutableBuilder.addText builder "\172735\1076202a"
                    MutableBuilder.addText builder ""
                    MutableBuilder.toText builder
            output @?= "\172735\1076202a"
                
        ]

mutableBuilderPropertyTests :: TestTree
mutableBuilderPropertyTests =
    Tasty.testGroup
        "MutableBuilder property tests"
        [ QuickCheck.testProperty "repeatedly appending arbitrary Texts behaves like mconcat" do
            \(strings :: [String]) -> do
                let texts = map Text.pack strings
                let combined = runST do
                        builder <- MutableBuilder.newWithCapacity 4
                        for_ texts \text -> MutableBuilder.addText builder text
                        MutableBuilder.toText builder
                combined === mconcat texts
        ]
