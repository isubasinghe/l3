{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CoreL3.Parser
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec $
  parallel $ do
    describe "should parse booleans" $ do
      it "parses true" $
        parse pbool "" "#t" `shouldParse` True
      it "parses false" $
        parse pbool "" "#f" `shouldParse` False

    describe "should parse integers" $ do
      it "parses positive integers" $
        parse pint "" "123" `shouldParse` 123
      it "parses negative integers" $
        parse pint "" "-123" `shouldParse` (-123)

    describe "should parse string literals" $ do
      it "parses string literals" $
        parse pstrlit "" "\"hello world\"" `shouldParse` "hello world"
      it "parses escaped string literals" $
        parse pstrlit "" "\"hello\\n\"" `shouldParse` "hello\n"

    describe "should parse identifiers" $ do
      it "parses C identifiers" $
        parse pidentifier "" "hello123" `shouldParse` "hello123"
      it "should not parse reserved keywords" $
        parse pidentifier "" `shouldFailOn` "begin"

    describe "should parse expressions" $ do
      it "parses and operations" $
        parse pand "" `shouldSucceedOn` "(and 123 123)"
      it "parses and operations where args > 0" $ do
        parse pand "" `shouldSucceedOn` "(and #f #t #t)"
