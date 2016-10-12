module Main where

import Test.Hspec
import Famihask
import Memory

main :: IO ()
main = hspec $ do
    describe "Memory" $ do
        it "words should be stored little-endian" $ do
            ram <- malloc 2
            storeWord ram 0 0x8cf3
            b0 <- loadByte ram 0
            b1 <- loadByte ram 1
            b0 `shouldBe` 0xf3
            b1 `shouldBe` 0x8c

        it "words should be loaded little-endian" $ do
            ram <- malloc 2
            storeByte ram 0 0xf3
            storeByte ram 1 0x8c
            w <- loadWord ram 0
            w `shouldBe` 0x8cf3
