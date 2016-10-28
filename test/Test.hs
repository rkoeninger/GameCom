module Main where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import Test.Hspec
import GameCom
import Memory
import CPU
import ROM

x |> f = f x

cpuFlagsShouldBe state flags value = forM_ flags testFlag
    where testFlag f = f state `shouldBe` value

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

    describe "ROM" $
        it "rom file header should begin with magic" $ do
            let bytes = [0x4e, 0x45, 0x53, 0x1a] ++ replicate 12 0x00
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

    describe "Arithmetic" $ do
        it "should perform simple, non-carried, non-overflow, addition" $ do
            let state = defaultState
                        |> setAReg 21 -- argument
                        |> storeByte 0x00 33 -- other argument
                        |> storeByte 0x10 0x65 -- adc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of adc/zpg
                        |> step
            aReg state `shouldBe` 54
            cpuFlagsShouldBe state [zeroFlag, negativeFlag, overflowFlag, carryFlag] False

        it "should perform simple, previous carry, non-overflow, addition" $ do
            let state = defaultState
                        |> setAReg 21 -- argument
                        |> setCarryFlag True
                        |> storeByte 0x00 33 -- other argument
                        |> storeByte 0x10 0x65 -- adc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of adc/zpg
                        |> step
            aReg state `shouldBe` 55
            cpuFlagsShouldBe state [zeroFlag, negativeFlag, overflowFlag, carryFlag] False

        it "should perform simple, non-carried, non-overflow, addition resulting in carry" $ do
            let state = defaultState
                        |> setAReg 150 -- argument
                        |> storeByte 0x00 150 -- other argument
                        |> storeByte 0x10 0x65 -- adc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of adc/zpg
                        |> step
            aReg state `shouldBe` 44
            cpuFlagsShouldBe state [zeroFlag, negativeFlag] False
            cpuFlagsShouldBe state [overflowFlag, carryFlag] True

        it "should perform simple, previous carry, non-overflow, addition resulting in carry" $ do
            let state = defaultState
                        |> setAReg 150 -- argument
                        |> setCarryFlag True
                        |> storeByte 0x00 150 -- other argument
                        |> storeByte 0x10 0x65 -- adc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of adc/zpg
                        |> step
            aReg state `shouldBe` 45
            cpuFlagsShouldBe state [zeroFlag, negativeFlag] False
            cpuFlagsShouldBe state [overflowFlag, carryFlag] True
