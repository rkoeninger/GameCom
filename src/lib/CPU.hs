module CPU where

import Data.Bits ((.|.), (.&.), xor, shiftL, shiftR, complement)
import Data.Word (Word8, Word16)
import Control.Monad (forM_)
import Memory

data Registers = Registers {
    accReg  :: Word8,
    xReg    :: Word8,
    yReg    :: Word8,
    sReg    :: Word8,
    flagReg :: Word8,
    pcReg   :: Word16
}

defaultRegs = Registers {
    accReg  = 0x00,
    xReg    = 0x00,
    yReg    = 0x00,
    sReg    = 0xfd,
    flagReg = 0x24,
    pcReg   = 0xc000
}

carryMask    = 0x01 :: Word8
zeroMask     = 0x02 :: Word8
irqMask      = 0x04 :: Word8
decimalMask  = 0x08 :: Word8
breakMask    = 0x10 :: Word8
overflowMask = 0x40 :: Word8
negativeMask = 0x80 :: Word8

nmiVector   = 0xfffa :: Word16
resetVector = 0xfffc :: Word16
breakVector = 0xfffe :: Word16

getFlag :: Word8 -> Registers -> Bool
getFlag mask regs = (flagReg regs .&. mask) /= 0

setFlag :: Word8 -> Bool -> Registers -> Registers
setFlag mask val regs =
    let flags = flagReg regs in
    regs {
        flagReg =
            if val
                then flags .|. mask
                else flags .&. (complement mask)
    }

setFlags :: Word8 -> Registers -> Registers
setFlags val regs = regs { flagReg = (val .|. 0x30) - 0x10 }

setZN :: Word8 -> Registers -> Registers
setZN val = (setFlag zeroMask $ val == 0) . (setFlag negativeMask $ (val .&. 0x80) /= 0)

-- FIXME: make use of cpuStore over Memory.store

cpuLoad :: Ram -> Word16 -> IO Word8
cpuLoad = loadByte

cpuStore :: Ram -> Word16 -> Word8 -> IO ()
cpuStore ram addr val =
    if addr == 0x4014
        then do -- handle DMA : what is this for?
            let start = (byteToWord val) `shiftL` 8
            let move ad = loadByte ram ad >>= storeByte ram 0x2004
            forM_ [start .. (start + 255)] move
        else storeByte ram addr val

loadByteIncPc :: Ram -> Registers -> IO (Registers, Word8)
loadByteIncPc ram regs = do
    let pc = pcReg regs
    val <- loadByte ram pc
    return (regs { pcReg = pc + 1 }, val)

loadWordIncPc :: Ram -> Registers -> IO (Registers, Word16)
loadWordIncPc ram regs = do
    let pc = pcReg regs
    val <- loadWord ram pc
    return (regs { pcReg = pc + 2 }, val)

pushByte :: Ram -> Registers -> Word8 -> IO Registers
pushByte ram regs val = do
    let s = sReg regs
    storeByte ram (0x0100 + (byteToWord s)) val
    return $ regs { sReg = s - 1 }

-- FIXME: Is this correct? FCEU has two self.storeb()s here. Might have different semantics...
pushWord :: Ram -> Registers -> Word16 -> IO Registers
pushWord ram regs val = do
    let s = sReg regs
    storeWord ram (0x0100 + (byteToWord s) - 1) val
    return $ regs { sReg = s - 2 }

popByte :: Ram -> Registers -> IO (Registers, Word8)
popByte ram regs = do
    let s = sReg regs
    val <- loadByte ram (0x0100 + (byteToWord s) + 1)
    return (regs { sReg = s + 1 }, val)

-- FIXME: see two functions up
popWord :: Ram -> Registers -> IO (Registers, Word16)
popWord ram regs = do
    let s = sReg regs
    val <- loadWord ram (0x0100 + (byteToWord s) + 1)
    return (regs { sReg = s + 2 }, val)

type Loader = Ram -> Registers -> IO (Registers, Word8)
type Storer = Ram -> Registers -> Word8 -> IO Registers
type Addresser = (Loader, Storer)
type AddresserBuilder = Ram -> Registers -> IO (Registers, Addresser)
type Instruction = Addresser -> Ram -> Registers -> IO Registers

nullMode :: AddresserBuilder
nullMode _ regs = return (regs, (loader, storer))
    where loader _ _ = error "this instruction should not access memory"
          storer _ _ _ = error "this instruction should not access memory"

accumulatorMode :: AddresserBuilder
accumulatorMode _ regs = return (regs, (loader, storer))
    where loader _ regs = return (regs, accReg regs)
          storer _ regs val = return $ regs { accReg = val }

immediateMode :: AddresserBuilder
immediateMode _ regs = return (regs, (loader, storer))
    where loader = loadByteIncPc
          storer _ _ _ = error "Can't store in immediate mode"

memoryMode :: Word16 -> Addresser
memoryMode addr = (loader, storer)
    where loader ram regs = do
              val <- loadByte ram addr
              return (regs, val)
          storer ram regs val = do
              storeByte ram addr val
              return regs

zeroPageMode :: AddresserBuilder
zeroPageMode ram regs = do
    (regs, addr) <- loadByteIncPc ram regs
    return (regs, memoryMode (byteToWord addr))

zeroPageXMode :: AddresserBuilder
zeroPageXMode ram regs = do
    (regs, addr) <- loadByteIncPc ram regs
    let addr = addr + (xReg regs)
    return (regs, memoryMode (byteToWord addr))

zeroPageYMode :: AddresserBuilder
zeroPageYMode ram regs = do
    (regs, addr) <- loadByteIncPc ram regs
    let addr = addr + (yReg regs)
    return (regs, memoryMode (byteToWord addr))

absoluteMode :: AddresserBuilder
absoluteMode ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    return (regs, memoryMode addr)

absoluteXMode :: AddresserBuilder
absoluteXMode ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    let addr = addr + byteToWord (xReg regs)
    return (regs, memoryMode addr)

absoluteYMode :: AddresserBuilder
absoluteYMode ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    let addr = addr + byteToWord (yReg regs)
    return (regs, memoryMode addr)

indexedIndirectXMode :: AddresserBuilder
indexedIndirectXMode ram regs = do
    (regs, addr) <- loadByteIncPc ram regs
    let x = xReg regs
    addr <- loadWord ram (byteToWord addr + byteToWord x) -- FIXME: this needs some alternate behavior for zero page mem.rs:35
    return (regs, memoryMode addr)

indirectIndexedYMode :: AddresserBuilder
indirectIndexedYMode ram regs = do
    (regs, addr) <- loadByteIncPc ram regs
    let y = yReg regs
    addr <- loadWord ram (byteToWord addr) -- FIXME: this needs some alternate behavior for zero page mem.rs:35
    let addr = addr + (byteToWord y)
    return (regs, memoryMode addr)

lda (loader, _) ram regs = do
    (regs, val) <- loader ram regs
    return $ (setZN val regs) { accReg = val }

ldx (loader, _) ram regs = do
    (regs, val) <- loader ram regs
    return $ (setZN val regs) { xReg = val }

ldy (loader, _) ram regs = do
    (regs, val) <- loader ram regs
    return $ (setZN val regs) { yReg = val }

sta (_, storer) ram regs = storer ram regs (accReg regs)
stx (_, storer) ram regs = storer ram regs (xReg regs)
sty (_, storer) ram regs = storer ram regs (yReg regs)

adc (loader, _) ram regs = do
    (regs, val) <- loader ram regs
    let result = (byteToWord (accReg regs)) + (byteToWord val) + (if getFlag carryMask regs then 1 else 0)
    let regs = setFlag carryMask ((result .&. 0x0100) /= 0) regs
    let result8 = wordToByte result
    let a = accReg regs
    let overflow = (((a `xor` val) .&. 0x80) == 0) && (((a `xor` result8) .&. 0x80) == 0x80)
    let regs = setFlag overflowMask overflow regs
    return $ (setZN result8 regs) { accReg = result8 }

sbc (loader, _) ram regs = do
    (regs, val) <- loader ram regs
    let a = accReg regs
    let result = (byteToWord a) - (byteToWord val) - (if getFlag carryMask regs then 0 else 1)
    let regs = setFlag carryMask ((result .&. 0x0100) == 0) regs
    let result8 = wordToByte result
    let overflow = (((a `xor` result8) .&. 0x80) /= 0) && (((a `xor` val) .&. 0x80) == 0x80)
    let regs = setFlag overflowMask overflow regs
    return $ (setZN result8 regs) { accReg = result8 }

compareHelper loader ram regs x = do
    (regs, y) <- loader ram regs
    let result = (byteToWord x) - (byteToWord y)
    let regs = setFlag carryMask ((result .&. 0x0100) == 0) regs
    return $ (setZN (wordToByte result) regs)

cmp (loader, _) ram regs = compareHelper loader ram regs (accReg regs)
cpx (loader, _) ram regs = compareHelper loader ram regs (xReg regs)
cpy (loader, _) ram regs = compareHelper loader ram regs (yReg regs)

bitwiseHelper loader ram regs f = do
    (regs, val) <- loader ram regs
    let val = val `f` accReg regs
    return $ (setZN val regs) { accReg = val }

add (loader, _) ram regs = bitwiseHelper loader ram regs (.&.)
ora (loader, _) ram regs = bitwiseHelper loader ram regs (.|.)
eor (loader, _) ram regs = bitwiseHelper loader ram regs xor

bit (loader, _) ram regs = do
    (regs, val) <- loader ram regs
    let a = accReg regs
    let regs = setFlag overflowMask ((val .&. 0x40) /= 0) .
               setFlag negativeMask ((val .&. 0x80) /= 0) .
               setFlag zeroMask ((val .&. a) == 0) $ regs
    return regs

shiftLeftHelper (loader, storer) ram regs lsb = do
    (regs, val) <- loader ram regs
    let carry = (val .&. 0x80) /= 0
    let result = (val `shiftL` 1) .|. (if lsb then 0x01 else 0x00)
    let regs = setZN result $ setFlag carryMask carry regs
    storer ram regs result

shiftRightHelper (loader, storer) ram regs msb = do
    (regs, val) <- loader ram regs
    let carry = (val .&. 0x01) /= 0
    let result = (val `shiftR` 1) .|. (if msb then 0x80 else 0x00)
    let regs = setZN result $ setFlag carryMask carry regs
    storer ram regs result

rol addresser ram regs = shiftLeftHelper addresser ram regs (getFlag carryMask regs)
ror addresser ram regs = shiftRightHelper addresser ram regs (getFlag carryMask regs)
asl addresser ram regs = shiftLeftHelper addresser ram regs False
lsr addresser ram regs = shiftRightHelper addresser ram regs False

inc (loader, storer) ram regs = do
    (regs, val) <- loader ram regs
    let val = val + 1
    storer ram (setZN val regs) val

dec (loader, storer) ram regs = do
    (regs, val) <- loader ram regs
    let val = val - 1
    storer ram (setZN val regs) val

inx _ _ regs = let x = xReg regs + 1 in return $ (setZN x regs) { xReg = x }
dex _ _ regs = let x = xReg regs - 1 in return $ (setZN x regs) { xReg = x }
iny _ _ regs = let y = yReg regs + 1 in return $ (setZN y regs) { yReg = y }
dey _ _ regs = let y = yReg regs - 1 in return $ (setZN y regs) { yReg = y }
tax _ _ regs = let acc = accReg regs in return $ (setZN acc regs) { xReg = acc }
tay _ _ regs = let acc = accReg regs in return $ (setZN acc regs) { yReg = acc }
txa _ _ regs = let x = xReg regs in return $ (setZN x regs) { accReg = x }
tya _ _ regs = let y = yReg regs in return $ (setZN y regs) { accReg = y }
txs _ _ regs = let x = xReg regs in return $ regs { sReg = x }
tsx _ _ regs = let s = sReg regs in return $ (setZN s regs) { xReg = s }
clc _ _ = return . setFlag carryMask False
sec _ _ = return . setFlag carryMask True
cli _ _ = return . setFlag irqMask False
sei _ _ = return . setFlag irqMask True
clv _ _ = return . setFlag overflowMask False
cld _ _ = return . setFlag decimalMask False
sed _ _ = return . setFlag decimalMask True

branch :: Ram -> Registers -> Bool -> IO Registers
branch ram regs cond =
    if cond
        then return regs
        else do
            (regs, disp) <- loadByteIncPc ram regs
            let pc = pcReg regs
            return $ regs { pcReg = pc + (byteToWord disp) }

bpl _ ram regs = branch ram regs $ not $ getFlag negativeMask regs
bmi _ ram regs = branch ram regs $ getFlag negativeMask regs
bvc _ ram regs = branch ram regs $ not $ getFlag overflowMask regs
bvs _ ram regs = branch ram regs $ getFlag overflowMask regs
bcc _ ram regs = branch ram regs $ not $ getFlag carryMask regs
bcs _ ram regs = branch ram regs $ getFlag carryMask regs
bne _ ram regs = branch ram regs $ not $ getFlag zeroMask regs
beq _ ram regs = branch ram regs $ getFlag zeroMask regs

jmp _ ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    return $ regs { pcReg = addr }

jpi _ ram regs = do
    (regs, addr) <- loadWordIncPc ram regs

    -- NOTE: apparently made necessary by bug in 6502 chip ???
    lo <- loadByte ram addr
    hi <- loadByte ram ((addr .&. 0xff00) .|. ((addr + 1) .&. 0x00ff))
    let addr = ((byteToWord hi) `shiftL` 8) .|. (byteToWord lo)

    return $ regs { pcReg = addr }

jsr _ ram regs = do
    (regs, addr) <- loadWordIncPc ram regs
    let pc = pcReg regs
    regs <- pushWord ram regs (pc - 1)
    return $ regs { pcReg = addr }

rts _ ram regs = do
    (regs, addr) <- popWord ram regs
    return $ regs { pcReg = addr + 1 }

brk _ ram regs = do
    let pc = pcReg regs
    regs <- pushWord ram regs (pc + 1)
    let flags = flagReg regs
    regs <- pushByte ram regs flags -- FIXME: FCEU sets BREAK_FLAG and U_FLAG here, why?
    let regs = setFlag irqMask True regs
    addr <- loadWord ram breakVector
    return $ regs { pcReg = addr }

rti _ ram regs = do
    (regs, flags) <- popByte ram regs
    let regs = setFlags flags regs
    (regs, addr) <- popWord ram regs
    return $ regs { pcReg = addr } -- NOTE: no +1

pha _ ram regs = pushByte ram regs (accReg regs)

pla _ ram regs = do
    (regs, val) <- popByte ram regs
    return $ (setZN val regs) { accReg = val }

php _ ram regs = pushByte ram regs (flagReg regs .|. breakMask)

plp _ ram regs = do
    (regs, flags) <- popByte ram regs
    return $ setFlags flags regs

nop _ _ regs = return regs

{-The main fetch-and-decode routine
    pub fn step(&mut self) {
        self.trace();

        let op = self.loadb_bump_pc();
        decode_op!(op, self);

        self.cy += CYCLE_TABLE[op as usize] as Cycles;
    }
-}

reset ram regs = do
    addr <- loadWord ram resetVector
    return (regs { pcReg = addr })

nmi ram regs = do
    let pc = pcReg regs
    let flags = flagReg regs
    regs <- pushWord ram regs pc
    regs <- pushByte ram regs flags
    addr <- loadWord ram nmiVector
    return $ regs { pcReg = addr }

irq ram regs =
    if getFlag irqMask regs
        then return regs
        else do
            let pc = pcReg regs
            let flags = flagReg regs
            regs <- pushWord ram regs pc
            regs <- pushByte ram regs flags
            addr <- loadWord ram breakVector
            return $ regs { pcReg = addr }

eval :: Ram -> Registers -> Word8 -> IO Registers
eval ram regs opCode = do
    let (op, builder) = decode opCode
    (regs, addresser) <- builder ram regs
    op addresser ram regs

decode :: Word8 -> (Instruction, AddresserBuilder)
decode 0xa1 = (lda, indexedIndirectXMode)
decode 0xa5 = (lda, zeroPageMode)
decode 0xa9 = (lda, immediateMode)
decode 0xad = (lda, absoluteMode)
decode 0xb1 = (lda, indirectIndexedYMode)
decode 0xb5 = (lda, zeroPageXMode)
decode 0xb9 = (lda, absoluteYMode)
decode 0xbd = (lda, absoluteXMode)
decode 0xa2 = (ldx, immediateMode)
decode 0xa6 = (ldx, zeroPageMode)
decode 0xb6 = (ldx, zeroPageYMode)
decode 0xae = (ldx, absoluteMode)
decode 0xbe = (ldx, absoluteYMode)
decode 0xa0 = (ldy, immediateMode)
decode 0xa4 = (ldy, zeroPageMode)
decode 0xb4 = (ldy, zeroPageXMode)
decode 0xac = (ldy, absoluteMode)
decode 0xbc = (ldy, absoluteXMode)
decode 0x85 = (sta, zeroPageMode)
decode 0x95 = (sta, zeroPageXMode)
decode 0x8d = (sta, absoluteMode)
decode 0x9d = (sta, absoluteXMode)
decode 0x99 = (sta, absoluteYMode)
decode 0x81 = (sta, indexedIndirectXMode)
decode 0x91 = (sta, indirectIndexedYMode)
decode 0x86 = (stx, zeroPageMode)
decode 0x96 = (stx, zeroPageYMode)
decode 0x8e = (stx, absoluteMode)
decode 0x84 = (sty, zeroPageMode)
decode 0x94 = (sty, zeroPageXMode)
decode 0x8c = (sty, absoluteMode)
decode 0x69 = (adc, immediateMode)
decode 0x65 = (adc, zeroPageMode)
decode 0x75 = (adc, zeroPageXMode)
decode 0x6d = (adc, absoluteMode)
decode 0x7d = (adc, absoluteXMode)
decode 0x79 = (adc, absoluteYMode)
decode 0x61 = (adc, indexedIndirectXMode)
decode 0x71 = (adc, indirectIndexedYMode)
decode 0xe9 = (sbc, immediateMode)
decode 0xe5 = (sbc, zeroPageMode)
decode 0xf5 = (sbc, zeroPageXMode)
decode 0xed = (sbc, absoluteMode)
decode 0xfd = (sbc, absoluteXMode)
decode 0xf9 = (sbc, absoluteYMode)
decode 0xe1 = (sbc, indexedIndirectXMode)
decode 0xf1 = (sbc, indirectIndexedYMode)
decode 0xc9 = (cmp, immediateMode)
decode 0xc5 = (cmp, zeroPageMode)
decode 0xd5 = (cmp, zeroPageXMode)
decode 0xcd = (cmp, absoluteMode)
decode 0xdd = (cmp, absoluteXMode)
decode 0xd9 = (cmp, absoluteYMode)
decode 0xc1 = (cmp, indexedIndirectXMode)
decode 0xd1 = (cmp, indirectIndexedYMode)
decode 0xe0 = (cpx, immediateMode)
decode 0xe4 = (cpx, zeroPageMode)
decode 0xec = (cpx, absoluteMode)
decode 0xc0 = (cpy, immediateMode)
decode 0xc4 = (cpy, zeroPageMode)
decode 0xcc = (cpy, absoluteMode)
decode 0x29 = (add, immediateMode)
decode 0x25 = (add, zeroPageMode)
decode 0x35 = (add, zeroPageXMode)
decode 0x2d = (add, absoluteMode)
decode 0x3d = (add, absoluteXMode)
decode 0x39 = (add, absoluteYMode)
decode 0x21 = (add, indexedIndirectXMode)
decode 0x31 = (add, indirectIndexedYMode)
decode 0x09 = (ora, immediateMode)
decode 0x05 = (ora, zeroPageMode)
decode 0x15 = (ora, zeroPageXMode)
decode 0x0d = (ora, absoluteMode)
decode 0x1d = (ora, absoluteXMode)
decode 0x19 = (ora, absoluteYMode)
decode 0x01 = (ora, indexedIndirectXMode)
decode 0x11 = (ora, indirectIndexedYMode)
decode 0x49 = (eor, immediateMode)
decode 0x45 = (eor, zeroPageMode)
decode 0x55 = (eor, zeroPageXMode)
decode 0x4d = (eor, absoluteMode)
decode 0x5d = (eor, absoluteXMode)
decode 0x59 = (eor, absoluteYMode)
decode 0x41 = (eor, indexedIndirectXMode)
decode 0x51 = (eor, indirectIndexedYMode)
decode 0x24 = (bit, zeroPageMode)
decode 0x2c = (bit, absoluteMode)
decode 0x2a = (rol, accumulatorMode)
decode 0x26 = (rol, zeroPageMode)
decode 0x36 = (rol, zeroPageXMode)
decode 0x2e = (rol, absoluteMode)
decode 0x3e = (rol, absoluteXMode)
decode 0x6a = (ror, accumulatorMode)
decode 0x66 = (ror, zeroPageMode)
decode 0x76 = (ror, zeroPageXMode)
decode 0x6e = (ror, absoluteMode)
decode 0x7e = (ror, absoluteXMode)
decode 0x0a = (asl, accumulatorMode)
decode 0x06 = (asl, zeroPageMode)
decode 0x16 = (asl, zeroPageXMode)
decode 0x0e = (asl, absoluteMode)
decode 0x1e = (asl, absoluteXMode)
decode 0x4a = (lsr, accumulatorMode)
decode 0x46 = (lsr, zeroPageMode)
decode 0x56 = (lsr, zeroPageXMode)
decode 0x4e = (lsr, absoluteMode)
decode 0x5e = (lsr, absoluteXMode)
decode 0xe6 = (inc, zeroPageMode)
decode 0xf6 = (inc, zeroPageXMode)
decode 0xee = (inc, absoluteMode)
decode 0xfe = (inc, absoluteXMode)
decode 0xc6 = (dec, zeroPageMode)
decode 0xd6 = (dec, zeroPageXMode)
decode 0xce = (dec, absoluteMode)
decode 0xde = (dec, absoluteXMode)
decode 0xe8 = (inx, nullMode)
decode 0xca = (dex, nullMode)
decode 0xc8 = (iny, nullMode)
decode 0x88 = (dey, nullMode)
decode 0xaa = (tax, nullMode)
decode 0xa8 = (tay, nullMode)
decode 0x8a = (txa, nullMode)
decode 0x98 = (tya, nullMode)
decode 0x9a = (txs, nullMode)
decode 0xba = (tsx, nullMode)
decode 0x18 = (clc, nullMode)
decode 0x38 = (sec, nullMode)
decode 0x58 = (cli, nullMode)
decode 0x78 = (sei, nullMode)
decode 0xb8 = (clv, nullMode)
decode 0xd8 = (cld, nullMode)
decode 0xf8 = (sed, nullMode)
decode 0x10 = (bpl, nullMode)
decode 0x30 = (bmi, nullMode)
decode 0x50 = (bvc, nullMode)
decode 0x70 = (bvs, nullMode)
decode 0x90 = (bcc, nullMode)
decode 0xb0 = (bcs, nullMode)
decode 0xd0 = (bne, nullMode)
decode 0xf0 = (beq, nullMode)
decode 0x4c = (jmp, nullMode)
decode 0x6c = (jpi, nullMode)
decode 0x20 = (jsr, nullMode)
decode 0x60 = (rts, nullMode)
decode 0x00 = (brk, nullMode)
decode 0x40 = (rti, nullMode)
decode 0x48 = (pha, nullMode)
decode 0x68 = (pla, nullMode)
decode 0x08 = (php, nullMode)
decode 0x28 = (plp, nullMode)
decode 0xea = (nop, nullMode)
decode opCode = error $ "Invalid op code: " ++ show opCode
