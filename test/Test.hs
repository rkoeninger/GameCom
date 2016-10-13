module Main where

import Test.Hspec
import Famihask
import Memory
import CPU

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

    describe "CPU" $ do
        it "negative numbers should set the negative flag in the status register" $ do
            let regs = setZN 0x80 defaultRegs
            (getFlag negativeMask regs) `shouldBe` True
            (getFlag zeroMask regs) `shouldBe` False

        it "zero should set the zero flag in the status register" $ do
            let regs = setZN 0x00 defaultRegs
            (getFlag zeroMask regs) `shouldBe` True
            (getFlag negativeMask regs) `shouldBe` False

        it "positive numbers should not set the zero or negative flags in the status register" $ do
            let regs = setZN 0x01 defaultRegs
            (getFlag zeroMask regs) `shouldBe` False
            (getFlag negativeMask regs) `shouldBe` False
