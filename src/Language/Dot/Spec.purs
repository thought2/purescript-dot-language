module Language.Dot.Spec where

import Prelude
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Language.Dot as D
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Undefined (undefined)

spec :: Spec Unit
spec = do
  describe "Language.Dot" do
    it "dd" do
      expected <- readTextFile UTF8 "golden/graph1.dot"
      let
        actual = D.printGraph undefined
      writeTextFile UTF8 "golden/graph1-actual.dot" actual
      expected `shouldEqual` actual
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let
          isAwesome = true
        isAwesome `shouldEqual` true
      pending "feature complete"
    describe "Features" do
      it "runs in NodeJS" $ pure unit
      it "runs in the browser" $ pure unit
      it "supports streaming reporters" $ pure unit
      it "supports async specs" do
        res <- delay (Milliseconds 100.0) $> "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.12.x compatible" $ pure unit
