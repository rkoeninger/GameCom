module Main where

import Control.Monad (forM_)
import Data.Bits ((.|.), (.&.), complement)
import qualified Data.ByteString as B
import Test.Hspec

import Base
import Memory
import qualified CPU
import ROM (Mirroring(..), Region(..), ROM(..), parseROM)
import GameCom

stackIs xs state = search 0 xs state
    where search _ [] _ = return ()
          search i (x : xs) s0 =
              let (b, s1) = CPU.pullByte s0 in
                  it ("stack[" ++ show i ++ "] should be " ++ show x) (b `shouldBe` x) >> search (i + 1) xs s1

pcRegIs   x state = it ("program counter should be " ++ show x) $ pcReg   state `shouldBe` x
flagRegIs x state = it ("flag register should be "   ++ show x) $ flagReg state `shouldBe` x
aRegIs    x state = it ("accumulator should be "     ++ show x) $ aReg    state `shouldBe` x
xRegIs    x state = it ("x register should be "      ++ show x) $ xReg    state `shouldBe` x
yRegIs    x state = it ("y register should be "      ++ show x) $ yReg    state `shouldBe` x
sRegIs    x state = it ("stack pointer should be"    ++ show x) $ sReg    state `shouldBe` x

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
                 3 -> 0x69 -- adc/imd
                 1 -> 0xe9 -- sbc/imd
    defaultState
        |> setAReg a
        |> setCarryFlag carry
        |> storeByte 0x10 opCode
        |> storeByte 0x11 val
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
        |> storeByte 0x10 0xc9 -- cmp/imd
        |> storeByte 0x11 val
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

testStack = describe "Stack Operations" $ do
    context "pushByte" $ do
        let state = defaultState
                    |> CPU.pushByte 0x12
                    |> CPU.pushByte 0x34
                    |> CPU.pushByte 0x56
                    |> CPU.pushByte 0x78
        stackIs [0x78, 0x56, 0x34, 0x12] state

    context "pushWord" $ do
        let state = defaultState
                    |> CPU.pushWord 0x1234
                    |> CPU.pushWord 0x5678
        stackIs [0x78, 0x56, 0x34, 0x12] state

    context "pla" $ do
        let state = defaultState
                    |> CPU.pushByte 0x7e
                    |> storeByte 0x10 0x68 -- pla
                    |> setPCReg 0x0010
                    |> CPU.step
        aRegIs 0x7e state

    context "plp" $ do
        let state = defaultState
                    |> CPU.pushByte 0x34
                    |> storeByte 0x10 0x28 -- plp
                    |> setPCReg 0x0010
                    |> CPU.step
        flagRegIs ((0x34 .|. unusedMask) .&. complement breakMask) state

    context "pha" $ do
        let state = defaultState
                    |> setAReg 0x7e
                    |> storeByte 0x10 0x48 -- pha
                    |> setPCReg 0x0010
                    |> CPU.step
        stackIs [0x7e] state

    context "php" $ do
        let state = defaultState
                    |> setFlagReg 0x34
                    |> storeByte 0x10 0x08 -- php
                    |> setPCReg 0x0010
                    |> CPU.step
        stackIs [0x34 .|. unusedMask] state

    context "rts" $ do
        let state = defaultState
                    |> CPU.pushWord 0x1234
                    |> storeByte 0x10 0x60 -- rts
                    |> setPCReg 0x0010
                    |> CPU.step
        pcRegIs 0x1235 state

    context "rti" $ do
        let state = defaultState
                    |> CPU.pushWord 0x1234
                    |> CPU.pushByte 0x34
                    |> storeByte 0x10 0x40 -- rti
                    |> setPCReg 0x0010
                    |> CPU.step
        pcRegIs 0x1234 state
        flagRegIs ((0x34 .|. unusedMask) .&. complement breakMask) state

main = hspec $ do
    testROM
    testMemory
    testArithmetic
    testComparisons
    testRotate
    testShift
    testStack
