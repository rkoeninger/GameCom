module Main where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import Test.Hspec
import GameCom
import Memory
import CPU hiding (step)
import qualified CPU (step)
import ROM

x |> f = f x

infixl 1 |>

negativeFlagSet   state = negativeFlag state `shouldBe` True
negativeFlagClear state = negativeFlag state `shouldBe` False
overflowFlagSet   state = overflowFlag state `shouldBe` True
overflowFlagClear state = overflowFlag state `shouldBe` False
zeroFlagSet       state = zeroFlag     state `shouldBe` True
zeroFlagClear     state = zeroFlag     state `shouldBe` False
carryFlagSet      state = carryFlag    state `shouldBe` True
carryFlagClear    state = carryFlag    state `shouldBe` False

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
            negativeFlagSet state
            zeroFlagClear state

        it "zero should set the zero flag in the status register" $ do
            let state = setZN 0x00 defaultState
            negativeFlagClear state
            zeroFlagSet state

        it "positive numbers should not set the zero or negative flags in the status register" $ do
            let state = setZN 0x01 defaultState
            negativeFlagClear state
            zeroFlagClear state

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
                        |> CPU.step
            aReg state `shouldBe` 54
            zeroFlagClear state
            negativeFlagClear state
            overflowFlagClear state
            carryFlagClear state

        it "should perform simple, previous carry, non-overflow, addition" $ do
            let state = defaultState
                        |> setAReg 21 -- argument
                        |> setCarryFlag True
                        |> storeByte 0x00 33 -- other argument
                        |> storeByte 0x10 0x65 -- adc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of adc/zpg
                        |> CPU.step
            aReg state `shouldBe` 55
            zeroFlagClear state
            negativeFlagClear state
            overflowFlagClear state
            carryFlagClear state

        it "should perform simple, non-carried, overflowing, addition resulting in carry" $ do
            let state = defaultState
                        |> setAReg 150 -- argument
                        |> storeByte 0x00 150 -- other argument
                        |> storeByte 0x10 0x65 -- adc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of adc/zpg
                        |> CPU.step
            aReg state `shouldBe` 44
            zeroFlagClear state
            negativeFlagClear state
            overflowFlagSet state
            carryFlagSet state

        it "should perform simple, previous carry, overflowing, addition resulting in carry" $ do
            let state = defaultState
                        |> setAReg 150 -- argument
                        |> setCarryFlag True
                        |> storeByte 0x00 150 -- other argument
                        |> storeByte 0x10 0x65 -- adc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of adc/zpg
                        |> CPU.step
            aReg state `shouldBe` 45
            zeroFlagClear state
            negativeFlagClear state
            overflowFlagSet state
            carryFlagSet state

        it "should perform simple, non-carried, non-overflow, subtraction" $ do
            let state = defaultState
                        |> setAReg 49 -- argument
                        |> storeByte 0x00 31 -- other argument
                        |> storeByte 0x10 0xe5 -- sbc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of sbc/zpg
                        |> CPU.step
            aReg state `shouldBe` 17
            zeroFlagClear state
            negativeFlagClear state
            overflowFlagSet state
            carryFlagSet state

        it "should perform simple, previous carry, non-overflow, subtraction" $ do
            let state = defaultState
                        |> setAReg 49 -- argument
                        |> setCarryFlag True
                        |> storeByte 0x00 31 -- other argument
                        |> storeByte 0x10 0xe5 -- sbc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of sbc/zpg
                        |> CPU.step
            aReg state `shouldBe` 18
            zeroFlagClear state
            negativeFlagClear state
            overflowFlagSet state
            carryFlagSet state

        it "should perform simple, non-carried, overflowing, subtraction resulting in a carry" $ do
            let state = defaultState
                        |> setAReg 49 -- argument
                        |> storeByte 0x00 81 -- other argument
                        |> storeByte 0x10 0xe5 -- sbc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of sbc/zpg
                        |> CPU.step
            aReg state `shouldBe` 223
            zeroFlagClear state
            negativeFlagSet state
            overflowFlagClear state
            carryFlagClear state

        it "should perform simple, previous carry, overflowing, subtraction resulting in a carry" $ do
            let state = defaultState
                        |> setAReg 49 -- argument
                        |> setCarryFlag True
                        |> storeByte 0x00 81 -- other argument
                        |> storeByte 0x10 0xe5 -- sbc/zpg instruction
                        |> storeByte 0x11 0x00 -- zpg address
                        |> setPCReg 0x0010 -- set PC to location of sbc/zpg
                        |> CPU.step
            aReg state `shouldBe` 224
            zeroFlagClear state
            negativeFlagSet state
            overflowFlagClear state
            carryFlagClear state
