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
        parse pexpr "" `shouldSucceedOn` "(and 123 123)"
      it "parses and operations where args > 0" $ do
        parse pexpr "" `shouldSucceedOn` "(and #f #t #t)"

    describe "should parse nested expressions" $ do
      it "parses a double nested expression" $
        parse pexpr "" `shouldSucceedOn` "(and #t (and #t #f))"
      it "parses a nested function application" $
        parse pexpr "" `shouldSucceedOn` "((fun (f x) (f x)) (fun (x) (@+ x 1)) 20)"

    describe "it should parse function declarations" $ do
      it "parses a zero arg fn decl" $
        parse pfun "" `shouldSucceedOn` "(fun () (and #t #t))"
      it "parses a 1 arg fn decl" $
        parse pfun "" `shouldSucceedOn` "(fun (x) (and #t #t))"

    describe "it should parse primitive ops" $ do
      it "parses add" $
        parse pprim "" `shouldSucceedOn` "(@+ 1 2)"
      it "parses minus" $
        parse pprim "" `shouldSucceedOn` "(@- 1 2 3 4)"
    
    describe "it shoud parse rec statements" $ do 
      it "parses a simple loop" $ 
        parse prec "" `shouldSucceedOn` "(rec loop ((i 1)) (intprint i) (if (@< i 9) (loop (@+ i 1))))"
