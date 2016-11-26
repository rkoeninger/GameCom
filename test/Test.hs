module Main where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import Test.Hspec
import GameCom
import Memory
import qualified CPU
import ROM (Mirroring(..), Region(..), ROM(..), parseROM)

aRegIs x state = it ("accumulator should be " ++ show x) $ aReg state `shouldBe` x

negativeFlagIs x state = it ("negative flag should be " ++ show x) $ negativeFlag state `shouldBe` x
overflowFlagIs x state = it ("overflow flag should be " ++ show x) $ overflowFlag state `shouldBe` x
zeroFlagIs     x state = it ("zero flag should be "     ++ show x) $ zeroFlag     state `shouldBe` x
carryFlagIs    x state = it ("carry flag should be "    ++ show x) $ carryFlag    state `shouldBe` x

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
        fst (loadByte 0 state) `shouldBe` 0xf3
        fst (loadByte 1 state) `shouldBe` 0x8c

    it "words should be loaded little-endian" $ do
        let state = storeByte 1 0x8c $ storeByte 0 0xf3 defaultState
        fst (loadWord 0 state) `shouldBe` 0x8cf3

arithmeticScenario a val carry f = do
    let opCode = case f 2 1 of
                 3 -> 0x65 -- adc/zpg
                 1 -> 0xe5 -- sbc/zpg
    defaultState
        |> setAReg a
        |> setCarryFlag carry
        |> storeByte 0x00 val
        |> storeByte 0x10 opCode
        |> storeByte 0x11 0x00
        |> setPCReg 0x0010
        |> CPU.step

testArithmetic = describe "Arithmetic" $ do
    context "should perform simple, non-carried, non-overflow, addition" $ do
        let state = arithmeticScenario 21 33 False (+)
        aRegIs 54 state
        zeroFlagIs False state
        negativeFlagIs False state
        overflowFlagIs False state
        carryFlagIs False state

    context "should perform simple, previous carry, non-overflow, addition" $ do
        let state = arithmeticScenario 21 33 True (+)
        aRegIs 55 state
        zeroFlagIs False state
        negativeFlagIs False state
        overflowFlagIs False state
        carryFlagIs False state

    context "should perform simple, non-carried, overflowing, addition resulting in carry" $ do
        let state = arithmeticScenario 150 150 False (+)
        aRegIs 44 state
        zeroFlagIs False state
        negativeFlagIs False state
        overflowFlagIs True state
        carryFlagIs True state

    context "should perform simple, previous carry, overflowing, addition resulting in carry" $ do
        let state = arithmeticScenario 150 150 True (+)
        aRegIs 45 state
        zeroFlagIs False state
        negativeFlagIs False state
        overflowFlagIs True state
        carryFlagIs True state

    context "should perform simple, non-carried, non-overflow, subtraction" $ do
        let state = arithmeticScenario 49 31 False (-)
        aRegIs 17 state
        zeroFlagIs False state
        negativeFlagIs False state
        overflowFlagIs False state
        carryFlagIs True state

    context "should perform simple, previous carry, non-overflow, subtraction" $ do
        let state = arithmeticScenario 49 31 True (-)
        aRegIs 18 state
        zeroFlagIs False state
        negativeFlagIs False state
        overflowFlagIs False state
        carryFlagIs True state

    context "should perform simple, non-carried, overflowing, subtraction resulting in a carry" $ do
        let state = arithmeticScenario 49 81 False (-)
        aRegIs 223 state
        zeroFlagIs False state
        negativeFlagIs True state
        overflowFlagIs False state
        carryFlagIs False state

    context "should perform simple, previous carry, overflowing, subtraction resulting in a carry" $ do
        let state = arithmeticScenario 49 81 True (-)
        aRegIs 224 state
        zeroFlagIs False state
        negativeFlagIs True state
        overflowFlagIs False state
        carryFlagIs False state

compareScenario a val =
    defaultState
        |> setAReg a
        |> storeByte 0x00 val
        |> storeByte 0x10 0xc5 -- cmp/zpg
        |> storeByte 0x11 0x00
        |> setPCReg 0x0010
        |> CPU.step

testComparisons = describe "Comparisons" $ do
    context "first argument is greater than second" $ do
        let state = compareScenario 1 0
        zeroFlagIs False state
        negativeFlagIs False state
        carryFlagIs False state

    context "first argument is less than second" $ do
        let state = compareScenario 0 1
        zeroFlagIs False state
        negativeFlagIs True state
        carryFlagIs True state

    context "first argument is equal to second" $ do
        let state = compareScenario 0 0
        zeroFlagIs True state
        negativeFlagIs False state
        carryFlagIs False state

rotateScenario a carry lr = do
    let opCode = case lr () of
                 Left  () -> 0x2a -- rol/acc
                 Right () -> 0x6a -- ror/acc
    defaultState
        |> setAReg a
        |> setCarryFlag carry
        |> storeByte 0x10 opCode
        |> setPCReg 0x0010
        |> CPU.step

testRotate = describe "Rotate" $ do
    context "when carry bit is clear, rotating left should leave right most bit clear" $ do
        let state = rotateScenario 0xff False Left
        aRegIs 0xfe state
        carryFlagIs True state
        zeroFlagIs False state
        negativeFlagIs True state

    context "when carry bit is set, rotating left should leave right most bit set" $ do
        let state = rotateScenario 0xff True Left
        aRegIs 0xff state
        carryFlagIs True state
        zeroFlagIs False state
        negativeFlagIs True state

    context "when carry bit is clear, rotating right should leave left most bit clear" $ do
        let state = rotateScenario 0xff False Right
        aRegIs 0x7f state
        carryFlagIs True state
        zeroFlagIs False state
        negativeFlagIs False state

    context "when carry bit is set, rotating right should leave left most bit set" $ do
        let state = rotateScenario 0xff True Right
        aRegIs 0xff state
        carryFlagIs True state
        zeroFlagIs False state
        negativeFlagIs True state

    context "when left most bit is clear, rotating left should clear carry bit" $ do
        let state = rotateScenario 0x7f False Left
        aRegIs 0xfe state
        carryFlagIs False state
        zeroFlagIs False state
        negativeFlagIs True state

    context "when right most bit is clear, rotating right should clear carry bit" $ do
        let state = rotateScenario 0xfe False Right
        aRegIs 0x7f state
        carryFlagIs False state
        zeroFlagIs False state
        negativeFlagIs False state

shiftScenario a carry lr = do
    let opCode = case lr () of
                 Left  () -> 0x0a -- asl/acc
                 Right () -> 0x4a -- lsr/acc
    defaultState
        |> setAReg a
        |> setCarryFlag carry
        |> storeByte 0x10 opCode
        |> setPCReg 0x0010
        |> CPU.step

testShift = describe "Shift" $ do
    context "when carry bit is clear, shifting left should leave right most bit clear" $ do
        let state = shiftScenario 0xff False Left
        aRegIs 0xfe state
        carryFlagIs True state
        zeroFlagIs False state
        negativeFlagIs True state

    context "when carry bit is set, shifting left should leave right most bit clear" $ do
        let state = shiftScenario 0xff True Left
        aRegIs 0xfe state
        carryFlagIs True state
        zeroFlagIs False state
        negativeFlagIs True state

    context "when carry bit is clear, shifting right should leave left most bit clear" $ do
        let state = shiftScenario 0xff False Right
        aRegIs 0x7f state
        carryFlagIs True state
        zeroFlagIs False state
        negativeFlagIs False state

    context "when carry bit is set, shifting right should leave left most bit clear" $ do
        let state = shiftScenario 0xff True Right
        aRegIs 0x7f state
        carryFlagIs True state
        zeroFlagIs False state
        negativeFlagIs False state

main = hspec $ do
    testROM
    testMemory
    testArithmetic
    testComparisons
    testRotate
    testShift
