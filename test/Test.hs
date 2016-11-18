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

aRegIs state val = it "accumulator should be" $ aReg state `shouldBe` val

negativeFlagSet   state = it "negative flag should be set"   $ negativeFlag state `shouldBe` True
negativeFlagClear state = it "negative flag should be clear" $ negativeFlag state `shouldBe` False
overflowFlagSet   state = it "overflow flag should be set"   $ overflowFlag state `shouldBe` True
overflowFlagClear state = it "overflow flag should be clear" $ overflowFlag state `shouldBe` False
zeroFlagSet       state = it "zero flag should be set"       $ zeroFlag     state `shouldBe` True
zeroFlagClear     state = it "zero flag should be clear"     $ zeroFlag     state `shouldBe` False
carryFlagSet      state = it "carry flag should be set"      $ carryFlag    state `shouldBe` True
carryFlagClear    state = it "carry flag should be clear"    $ carryFlag    state `shouldBe` False

testROM = describe "ROM" $
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

testMemory = describe "Memory" $ do
    it "words should be stored little-endian" $ do
        let state = storeWord 0 0x8cf3 defaultState
        loadByte 0 state `shouldBe` 0xf3
        loadByte 1 state `shouldBe` 0x8c

    it "words should be loaded little-endian" $ do
        let state = storeByte 1 0x8c $ storeByte 0 0xf3 defaultState
        loadWord 0 state `shouldBe` 0x8cf3

testArithmetic = describe "Arithmetic" $ do
    context "should perform simple, non-carried, non-overflow, addition" $ do
        let state = defaultState
                    |> setAReg 21 -- argument
                    |> storeByte 0x00 33 -- other argument
                    |> storeByte 0x10 0x65 -- adc/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of adc/zpg
                    |> CPU.step
        aRegIs state 54
        zeroFlagClear state
        negativeFlagClear state
        overflowFlagClear state
        carryFlagClear state

    context "should perform simple, previous carry, non-overflow, addition" $ do
        let state = defaultState
                    |> setAReg 21 -- argument
                    |> setCarryFlag True
                    |> storeByte 0x00 33 -- other argument
                    |> storeByte 0x10 0x65 -- adc/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of adc/zpg
                    |> CPU.step
        aRegIs state 55
        zeroFlagClear state
        negativeFlagClear state
        overflowFlagClear state
        carryFlagClear state

    context "should perform simple, non-carried, overflowing, addition resulting in carry" $ do
        let state = defaultState
                    |> setAReg 150 -- argument
                    |> storeByte 0x00 150 -- other argument
                    |> storeByte 0x10 0x65 -- adc/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of adc/zpg
                    |> CPU.step
        aRegIs state 44
        zeroFlagClear state
        negativeFlagClear state
        overflowFlagSet state
        carryFlagSet state

    context "should perform simple, previous carry, overflowing, addition resulting in carry" $ do
        let state = defaultState
                    |> setAReg 150 -- argument
                    |> setCarryFlag True
                    |> storeByte 0x00 150 -- other argument
                    |> storeByte 0x10 0x65 -- adc/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of adc/zpg
                    |> CPU.step
        aRegIs state 45
        zeroFlagClear state
        negativeFlagClear state
        overflowFlagSet state
        carryFlagSet state

    context "should perform simple, non-carried, non-overflow, subtraction" $ do
        let state = defaultState
                    |> setAReg 49 -- argument
                    |> storeByte 0x00 31 -- other argument
                    |> storeByte 0x10 0xe5 -- sbc/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of sbc/zpg
                    |> CPU.step
        aRegIs state 17
        zeroFlagClear state
        negativeFlagClear state
        overflowFlagClear state
        carryFlagSet state

    context "should perform simple, previous carry, non-overflow, subtraction" $ do
        let state = defaultState
                    |> setAReg 49 -- argument
                    |> setCarryFlag True
                    |> storeByte 0x00 31 -- other argument
                    |> storeByte 0x10 0xe5 -- sbc/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of sbc/zpg
                    |> CPU.step
        aRegIs state 18
        zeroFlagClear state
        negativeFlagClear state
        overflowFlagClear state
        carryFlagSet state

    context "should perform simple, non-carried, overflowing, subtraction resulting in a carry" $ do
        let state = defaultState
                    |> setAReg 49 -- argument
                    |> storeByte 0x00 81 -- other argument
                    |> storeByte 0x10 0xe5 -- sbc/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of sbc/zpg
                    |> CPU.step
        aRegIs state 223
        zeroFlagClear state
        negativeFlagSet state
        overflowFlagClear state
        carryFlagClear state

    context "should perform simple, previous carry, overflowing, subtraction resulting in a carry" $ do
        let state = defaultState
                    |> setAReg 49 -- argument
                    |> setCarryFlag True
                    |> storeByte 0x00 81 -- other argument
                    |> storeByte 0x10 0xe5 -- sbc/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of sbc/zpg
                    |> CPU.step
        aRegIs state 224
        zeroFlagClear state
        negativeFlagSet state
        overflowFlagClear state
        carryFlagClear state

testComparisons = describe "Comparisons" $ do
    context "first argument is greater than second" $ do
        let state = defaultState
                    |> setAReg 1 -- argument
                    |> storeByte 0x00 0 -- other argument
                    |> storeByte 0x10 0xc5 -- cmp/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of cmp/zpg
                    |> CPU.step
        zeroFlagClear state
        negativeFlagClear state
        carryFlagClear state

    context "first argument is less than second" $ do
        let state = defaultState
                    |> setAReg 0 -- argument
                    |> storeByte 0x00 1 -- other argument
                    |> storeByte 0x10 0xc5 -- cmp/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of cmp/zpg
                    |> CPU.step
        zeroFlagClear state
        negativeFlagSet state
        carryFlagSet state

    context "first argument is equal to second" $ do
        let state = defaultState
                    |> setAReg 0 -- argument
                    |> storeByte 0x00 0 -- other argument
                    |> storeByte 0x10 0xc5 -- cmp/zpg instruction
                    |> storeByte 0x11 0x00 -- zpg address
                    |> setPCReg 0x0010 -- set PC to location of cmp/zpg
                    |> CPU.step
        zeroFlagSet state
        negativeFlagClear state
        carryFlagClear state

main :: IO ()
main = hspec $ do
    testROM
    testMemory
    testArithmetic
    testComparisons
