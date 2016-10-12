module CPU where

import Data.Bits ((.|.), (.&.), complement)
import Data.Word (Word8, Word16)
import Memory

data Registers = Registers {
    accReg :: Word8,
    xReg :: Word8,
    yReg :: Word8,
    sReg :: Word8,
    flagReg :: Word8,
    pcReg :: Word16
}

carryMask    = 0x01 :: Word8
zeroMask     = 0x02 :: Word8
irqMask      = 0x04 :: Word8
decimalMask  = 0x08 :: Word8
breakMask    = 0x10 :: Word8
overflowMask = 0x40 :: Word8
negativeMask = 0x80 :: Word8

getFlag :: Word8 -> Registers -> Bool
getFlag mask regs = ((flagReg regs) .&. mask) == 0

setFlag :: Word8 -> Bool -> Registers -> Registers
setFlag mask val regs =
    let current = flagReg regs in
    regs {
        flagReg =
            if val
                then current .|. mask
                else current .&. (complement mask)
    }

setFlags :: Word8 -> Registers -> Registers
setFlags val regs = regs { flagReg = (val .|. 0x30) - 0x10 }

setZN :: Word8 -> Registers -> Registers
setZN val = (setFlag zeroMask $ val == 0) . (setFlag negativeMask $ (val .&. 0x80) /= 0)

data AddressMode = AccumulatorMode | ImmediateMode | MemoryMode

lda AccumulatorMode regs ram = return ()
lda ImmediateMode regs ram = return ()
lda MemoryMode regs ram = return ()

inx regs = let x = xReg regs + 1 in (setZN x regs) { xReg = x }
dex regs = let x = xReg regs - 1 in (setZN x regs) { xReg = x }
iny regs = let y = yReg regs + 1 in (setZN y regs) { yReg = y }
dey regs = let y = yReg regs - 1 in (setZN y regs) { yReg = y }
tax regs = let acc = accReg regs in (setZN acc regs) { xReg = acc }
tay regs = let acc = accReg regs in (setZN acc regs) { yReg = acc }
txa regs = let x = xReg regs in (setZN x regs) { accReg = x }
tya regs = let y = yReg regs in (setZN y regs) { accReg = y }
txs regs = let x = xReg regs in regs { sReg = x }
tsx regs = let s = sReg regs in (setZN s regs) { xReg = s }
clc = setFlag carryMask False
sec = setFlag carryMask True
cli = setFlag irqMask False
sei = setFlag irqMask True
clv = setFlag overflowMask False
cld = setFlag decimalMask False
sed = setFlag decimalMask True
