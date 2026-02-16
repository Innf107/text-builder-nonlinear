{-# LANGUAGE BlockArguments #-}
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck qualified as QuickCheck

import Builder qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.QuickCheck ((===))
import Builder (Builder(MkBoxedBuilder))

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Tests" [builderUnitTests, builderPropertyTests]

builderUnitTests :: TestTree
builderUnitTests =
    Tasty.testGroup
        "Builder Unit tests"
        [ HUnit.testCase "empty builder" $ Builder.toText (Builder.unboxBuilder Builder.empty) @=? ""
        , HUnit.testCase "linear short" do
            let builder = Builder.addText "ccc" $ Builder.addText "bbb" $ Builder.addText "aaa" $ Builder.unboxBuilder (Builder.empty)
            Builder.toText builder @=? "aaabbbccc"
        , HUnit.testCase "linear fixed chunk" do
            let builder = Builder.addText (Text.replicate 1000 "x") $ Builder.addText "short" (Builder.unboxBuilder (Builder.empty))
            Builder.toText builder @=? ("short" <> Text.replicate 1000 "x")
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
        [ QuickCheck.testProperty "Linearly appending arbitrary Texts behaves like mconcat" $
            -- Manual fold because we're working with an unboxed builder here
            let appendTexts builder = \case
                    [] -> Builder.toText builder
                    (text : rest) -> appendTexts (Builder.addText text builder) rest
             in \(strings :: [String]) -> do
                    let texts = map Text.pack strings
                    mconcat texts === appendTexts (Builder.unboxBuilder Builder.empty) texts
        ]
