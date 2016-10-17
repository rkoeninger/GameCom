module Main where

import qualified Data.ByteString as B
import Test.Hspec
import Famihask
import Memory (storeWord, storeByte, loadWord, loadByte, malloc)
import CPU (defaultRegs, getFlag, negativeMask, zeroMask, setZN)
import ROM

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

    describe "ROM" $ do
        it "rom file header should begin with magic" $ do
            let bytes = [0x4e, 0x45, 0x53, 0x1a] ++ take 12 (repeat 0x00)
            let rom = ROM {
                mirroring = Horizontal,
                trainer = False,
                persistent = False,
                inesMapper = 0,
                mapper = 0,
                playChoice = False,
                unisystem = False,
                ramSize = 0,
                region = NTSC,
                prg = B.empty,
                chr = B.empty
            }
            parseROM (B.pack bytes) `shouldBe` Right rom
