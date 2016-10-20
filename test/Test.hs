module Main where

import qualified Data.ByteString as B
import Test.Hspec
import GameCom
import Memory
import CPU
import ROM

main :: IO ()
main = hspec $ do
    describe "Memory" $ do
        it "words should be stored little-endian" $ do
            let state = storeWord 0 0x8cf3 defaultState
            loadByte 0 state `shouldBe` 0xf3
            loadByte 1 state `shouldBe` 0x8c

        it "words should be loaded little-endian" $ do
            let state = storeByte 1 0x8c $ storeByte 0 0xf3 defaultState
            loadWord 0 state `shouldBe` 0x8cf3

    describe "CPU" $ do
        it "negative numbers should set the negative flag in the status register" $ do
            let state = setZN 0x80 defaultState
            negativeFlag state `shouldBe` True
            zeroFlag state `shouldBe` False

        it "zero should set the zero flag in the status register" $ do
            let state = setZN 0x00 defaultState
            negativeFlag state `shouldBe` False
            zeroFlag state `shouldBe` True

        it "positive numbers should not set the zero or negative flags in the status register" $ do
            let state = setZN 0x01 defaultState
            negativeFlag state `shouldBe` False
            zeroFlag state `shouldBe` False

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
