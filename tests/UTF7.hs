{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.Instances ()

import qualified Data.ByteString as B
import qualified Data.Text       as T
import Control.Applicative

import Data.Text.Encoding.UTF7.IMAP

infix 4 <==>
(<==>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(<==>) = liftA2 (==)

main = defaultMain $ testGroup "Tests" [
  testGroup "Modified UTF-7" [
    testGroup "Encoding" [
      testCase "ASCII text passes straight through" $
        encodeUtf7 "INBOX" @?= "INBOX",
      testCase "Simple inline accented characters" $
        encodeUtf7 "Boîte de réception" @?= "Bo&AO4-te de r&AOk-ception",
      testCase "Some Japanese Unicode text" $
        encodeUtf7 "受信トレイ" @?= "&U9dP4TDIMOwwpA-",
      testCase "Strings which end on the ninth byte" $
        encodeUtf7 "\n\ná" @?= "&AAoACgDh-",
      testCase "Characters which don't fit into one UTF16 word" $
        encodeUtf7 "🎶🏁🏇" @?= "&2Dzfttg838HYPN,H-",
      testCase "String containing potentially problematic characters" $
        encodeUtf7 "This&That-" @?= "This&-That-"],
    testGroup "Decoding" [
      testCase "ASCII text passes straight through" $
        decodeUtf7 "INBOX" @?= "INBOX",
      testCase "Simple inline accented characters" $
        decodeUtf7 "Bo&AO4-te de r&AOk-ception" @?= "Boîte de réception",
      testCase "Some Japanese Unicode text" $
        decodeUtf7 "&U9dP4TDIMOwwpA-" @?= "受信トレイ",
      testCase "Strings which end on the ninth byte" $
        decodeUtf7 "&AAoACgDh-" @?= "\n\ná",
      testCase "Characters which don't fit into one UTF16 word" $
        decodeUtf7 "&2Dzfttg838HYPN,H-" @?= "🎶🏁🏇",
      testCase "String containing potentially problematic characters" $
        decodeUtf7 "This&-That-" @?= "This&That-"],
    testGroup "Properties" [
      testProperty "decodeUtf7 . encodeUtf7 = id" $
        decodeUtf7 . encodeUtf7 <==> id,
      testProperty "No NUL characters in output" $
        \a -> 0 `B.notElem` encodeUtf7 a]]]
